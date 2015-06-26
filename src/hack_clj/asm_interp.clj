(ns hack-clj.asm-interp
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [hack-clj.assembler :as asm]
            [hack-clj.parse :refer [int->binstring binstring->int flip-bin-chars]]))

(defn parse-instruction
  "Given a de-sugared ASM instruction, return a variant of the form [type data].
  Types: :a-instruction, :jump, :assignment"
  [instruction]
  (let [type (asm/instruction-type instruction)
        data (case type
               :a-instruction (-> instruction
                                (subs 1)
                                (Integer/parseInt))
               :c-instruction (asm/parse-c-instruction instruction)
               (throw (IllegalArgumentException.
                       (str instruction " is unsupported type: " (name type) "."))))]
    [type data]))

;; TODO come up with good, differentiated names for the stages in interpretation
;; we've got sugary assembly code -> desugared assembly -> parsed assembly -> interpretable assembly

(defn instruction->interpretable
  "Parses the given instruction string into an interpretable statement."
  [instruction]
  (let [[type data] (parse-instruction instruction)]
    (cond
      (= type :a-instruction)
      [:a-instruction data]

      (and (= type :c-instruction)
           (contains? data :jump))
      [:jump data]

      (and (= type :c-instruction))
      [:assignment data]

      :else
      (throw (IllegalArgumentException. (str "Can't make an interpretable instruction from " instruction))))))

(defn remove-end-loop
  "The convention is for hack programs to 'end' in an infinite loop. This
  function detects such a loop and removes the last instruction (an
  unconditional jump) so that the interpreter will just terminate instead.

  Won't affect programs that don't use this convention."
  [interpretable-program]
  (let [last-two (fn [coll] (let [v (vec coll)
                                  c (count v)]
                              (subvec v (- c 2))))
        line-x-inst (mapv vector (range) interpretable-program)
        ;; line-x-inst is a vector of [line-number instruction] pairs.
        ;;   where an instruction is itself a pair of [type data]
        ;;   and data will be an int for :a-instruction, or a map
        ;;   for :assignment or :jump
        [penult ult] (last-two line-x-inst)
        penult-refers-to-self? (let [[line [type data]] penult]
                                 (and (= :a-instruction type)
                                      (= line data)))
        ult-jumps-to-penult? (let [[_ [type data]] ult]
                               (and (= :jump type)
                                    (= "JMP" (:jump data))))]
    (if (and penult-refers-to-self?
             ult-jumps-to-penult?)
      (vec (butlast interpretable-program))
      interpretable-program)))

(defn get-register
  [ram reg]
  (case reg
    :A (get-in ram [:registers :A] 0)
    :D (get-in ram [:registers :D] 0)
    :M (let [address (get-register ram :A)]
         (get ram address 0))))

(defn set-register
  [ram reg val]
  (case reg
    :A (assoc-in ram [:registers :A] val)
    :D (assoc-in ram [:registers :D] val)
    :M (let [address (get-register ram :A)]
         (assoc ram address val))))

(defn set-registers
  "Given the RAM, a string/keyword like 'A'/:AMD, and a value, assoc the value into
  the named registers."
  [ram regs val]
  (let [registers (->> regs name (map (comp keyword str)) (into #{}))
        A? (:A registers)
        D? (:D registers)
        M? (:M registers)]
    (cond-> ram
      A? (set-register :A val)
      D? (set-register :D val)
      M? (set-register :M val))))

(defn b-not
  "Int -> Int. 16-bit binary NOT."
  [n]
  (->> n
    int->binstring
    (map {\0 \1, \1 \0})
    (apply str)
    binstring->int))

(defn b-and
  "Int Int -> Int. 16-bit binary AND."
  [x y]
  (let [xb (int->binstring x)
        yb (int->binstring y)
        bit-and (fn [x y] (if (= \1 x y) "1" "0"))]
    (->> (map bit-and xb yb)
      (apply str)
      binstring->int)))

(defn b-or
  "Int Int -> Int. 16-bit binary OR."
  [x y]
  (let [xb (int->binstring x)
        yb (int->binstring y)
        bit-or (fn [x y] (if (or (= \1 x)
                                 (= \1 y))
                           "1"
                           "0"))]
    (->> (map bit-or xb yb)
      (apply str)
      binstring->int)))

(defn compute
  "Given the RAM map and the comp field of a c-instruction, return the resulting
  value."
  [ram comp]
  (let [r-get (partial get-register ram)
        A (r-get :A)
        D (r-get :D)
        M (r-get :M)]
    (case comp
      "0"   0
      "1"   1
      "-1"  -1
      "D"   D
      "A"   A
      "!D"  (b-not D)
      "!A"  (b-not A)
      "-D"  (- D)
      "-A"  (- A)
      "D+1" (inc D)
      "A+1" (inc A)
      "D-1" (dec D)
      "A-1" (dec A)
      "D+A" (+ D A)
      "D-A" (- D A)
      "A-D" (- A D)
      "D&A" (b-and D A)
      "D|A" (b-or D A)
      "M"   M
      "!M"  (b-not M)
      "-M"  (- M)
      "M+1" (inc M)
      "M-1" (dec M)
      "D+M" (+ D M)
      "D-M" (- D M)
      "M-D" (- M D)
      "D&M" (b-and D M)
      "D|M" (b-or D M)
      (throw (IllegalArgumentException. (str "Can't compute " comp " :: " (type comp)))))))

(defn jump?
  [comparison val]
  (case comparison
    "JGT" (pos? val)
    "JEQ" (zero? val)
    "JGE" (>= val 0)
    "JLT" (neg? val)
    "JNE" (not (zero? val))
    "JLE" (<= val 0)
    "JMP" true
    (throw (IllegalArgumentException. (str comparison " doesn't appear to be a jump instruction.")))))

(defn run*
  ([program] (run* program {}))
  ([program init-vars]
   (let [instructions (->> program
                        (mapv instruction->interpretable)
                        remove-end-loop)
         init-mem (merge init-vars {:registers {:A 0, :D 0}})]
     (loop [pc 0
            ram init-mem
            fuel 1e7] ;; i.e., about 10 seconds in the REPL on my machine
       (if-let [[type instruction] (get instructions pc)]
         (if (zero? fuel) ;; to ensure termination
           ram
           (case type
             :a-instruction (recur (inc pc)
                                   (set-register ram :A instruction)
                                   (dec fuel))

             :assignment (let [val (compute ram (:comp instruction))
                               dest (:dest instruction)]
                           (recur (inc pc)
                                  (set-registers ram dest val)
                                  (dec fuel)))

             :jump (let [val (compute ram (:comp instruction))
                         comparison (:jump instruction)
                         next-pc (if (jump? comparison val)
                                   (get-register ram :A)
                                   (inc pc))]
                     (recur next-pc
                            ram
                            (dec fuel)))))
         ;; else we're out of instructions
         ram)))))

{:registers {:A 0, :D 0, :PC 0}
 :program []}

(defn map-keys
  [f m]
  (->> m
    (map (fn [[k v]] [(f k) v]))
    (filter (fn [[k v]] (some? k)))
    (into {})))

(defn run
  "Given a program (which may include syntactic sugar), return the resulting
  memory map along with a map of variables with their final values."
  ;; TODO make it possible to bind vars by name in init-vars.
  ;;   E.g.: {"in" 300} -> {16 300}
  ([hack-asm] (run hack-asm {}))
  ([hack-asm init-vars]
   (let [sym-map (-> hack-asm
                   asm/symbol-map
                   (dissoc "SP" "LCL" "ARG" "THIS" "THAT") ; these conflict with R0-R4
                   set/map-invert)
         desugared (asm/lines->instructions hack-asm)
         memory (run* desugared init-vars)
         final-vars (map-keys sym-map memory)]
     (assoc memory :vars final-vars))))

;; TODO
;; consider using a vector for ram

;; Experimental lazy seq of states

(defn next-state
  [state]
  (let [{:keys [program registers]} state]
    (if-let [[type instruction] (get program (:PC registers))]
      (case type
        :a-instruction (-> state
                         (update-in [:registers :PC] inc)
                         (set-register :A instruction))
        :assignment (let [val (compute state (:comp instruction))
                          dest (:dest instruction)]
                      (-> state
                        (update-in [:registers :PC] inc)
                        (set-registers dest val)))
        :jump (let [val (compute state (:comp instruction))
                    comparison (:jump instruction)
                    next-pc (if (jump? comparison val)
                              (get-register state :A)
                              (inc (get-in state [:registers :PC])))]
                (assoc-in state [:registers :PC] next-pc))))))

(defn state-seq
  [state]
  (lazy-cat
   [state]
   (if-let [next (next-state state)]
     (state-seq next)
     [])))

(defn run-states
  ([hack-asm] (run-states hack-asm {}))
  ([hack-asm init-mem]
   (let [program (->> hack-asm
                   asm/lines->instructions
                   (mapv instruction->interpretable)
                   remove-end-loop)
         init-state (-> {:registers {:A 0, :D 0, :PC 0}
                         :program program}
                      (merge init-mem))]
     (state-seq init-state))))
