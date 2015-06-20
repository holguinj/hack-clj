(ns hack-clj.assembler
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn zfill
  "Returns a string ending with s, padded with zeroes up to n
  characters."
  ([s n] (let [length (- n (count s))
               fill (apply str (repeat length "0"))]
           (str fill s)))
  ([s] (zfill s 15)))

(defn binary
  [n]
  (-> n
    str
    (Integer/parseInt)
    (Integer/toString 2)))

(def extract
  (comp second (partial re-find)))

(def get-dest (partial extract #"(\w+)="))
(def get-jump (partial extract #";(\w+)"))
(defn get-comp [s]
  (let [comp-syms #"([AMD01\!\|\-\&\+]+)"]
    (or (extract (re-pattern (str "=" comp-syms)) s)
        (extract (re-pattern (str comp-syms ";")) s)
        (throw (IllegalArgumentException.
                (str "get-comp called with unparseable input: " s))))))

(defn parse-c-instruction
  [s]
  (let [dest? (re-find #"=" s)
        jump? (re-find #";" s)]
    (merge {:comp (get-comp s)}
           (if dest? {:dest (get-dest s)})
           (if jump? {:jump (get-jump s)}))))

(def comp-instruction
  ;; a=0
  {"0"   "0101010"
   "1"   "0111111"
   "-1"  "0111010"
   "D"   "0001100"
   "A"   "0110000"
   "!D"  "0001101"
   "!A"  "0110001"
   "-D"  "0001111"
   "-A"  "0110011"
   "D+1" "0011111"
   "A+1" "0110111"
   "D-1" "0001110"
   "A-1" "0110010"
   "D+A" "0000010"
   "D-A" "0010011"
   "A-D" "0000111"
   "D&A" "0000000"
   "D|A" "0010101"

   ;; a=1
   "M"   "1110000"
   "!M"  "1110001"
   "-M"  "1110011"
   "M+1" "1110111"
   "M-1" "1110010"
   "D+M" "1000010"
   "D-M" "1010011"
   "M-D" "1000111"
   "D&M" "1000000"
   "D|M" "1010101"})

(def dest-instruction
  {"M"   "001"
   "D"   "010"
   "MD"  "011"
   "A"   "100"
   "AM"  "101"
   "AD"  "110"
   "AMD" "111"})

(def jump-instruction
  {"JGT" "001"
   "JEQ" "010"
   "JGE" "011"
   "JLT" "100"
   "JNE" "101"
   "JLE" "110"
   "JMP" "111"})

(defn generate-c-instruction
  [{:keys [dest comp jump]}]
  (str "111"
       (comp-instruction comp)
       (get dest-instruction dest "000")
       (get jump-instruction jump "000")))

(defn instruction-type
  [s]
  (cond
    (re-find #"@\d+$" s)   :a-instruction
    (re-find #"^.+[=;]" s) :c-instruction
    (re-find #"\(.+\)$" s) :jump-target
    (re-find #"@\S+$" s)   :a-var
    :else (throw (IllegalArgumentException.
                  (str "unknown instruction type " s)))))

(defn assert-output
  [in out]
  (if (and (= 16 (count out))
           (re-matches #"^[01]+$" out))
    out
    (throw (IllegalArgumentException. (str "Compilation failure: " in " -> " out)))))

(defn compile-c-instruction
  "Parse the given string as a C-instruction and return the resulting
  binary string."
  [s]
  (-> s
    parse-c-instruction
    generate-c-instruction))

(defn compile-a-instruction
  [num]
  (let [num-str (str/replace (str num) #"@" "")
        val (binary num-str)]
    (->> val
      zfill
      (str "0"))))

(defn compile-instruction
  [s]
  {:post [(assert-output s %)]}
  (condp = (instruction-type s)
    :a-instruction (compile-a-instruction s)
    :c-instruction (compile-c-instruction s)
    (throw (IllegalArgumentException.
            (str "Can't compile instruction: " s " of type "
                 (instruction-type s) ".")))))

(defn code?
  "Returns true if the given string is not a comment and not blank"
  [s]
  (let [line (-> s (str/replace #"//.*$" "") str/trim)]
    (not (str/blank? line))))

(defn a-var?
  [s]
  (= :a-var
     (instruction-type s)))

(defn jump?
  [s]
  (= :jump-target
     (instruction-type s)))

(defn strip-parens
  [s]
  (str/replace s #"[\(\)]" ""))

(defn jump-map
  [instructions]
  (loop [[inst :as remaining] instructions
         acc {}
         i 0]
    (cond
      (empty? remaining) acc

      (jump? inst)
      (recur (rest remaining)
             (assoc acc (strip-parens inst) i)
             i)

      :else
      (recur (rest remaining)
             acc
             (inc i)))))

(def base-symbols
  {"SP" 0
   "LCL" 1
   "ARG" 2
   "THIS" 3
   "THAT" 4
   "R0" 0 "R1" 1
   "R2" 2 "R3" 3
   "R4" 4 "R5" 5
   "R6" 6 "R7" 7
   "R8" 8 "R9" 9
   "R10" 10 "R11" 11
   "R12" 12 "R13" 13
   "R14" 14 "R15" 15
   "SCREEN" 16384
   "KBD" 24576})

(defn var-map
  [jumps instructions]
  (let [BASE 16
        vars (->> instructions
               (filter a-var?)
               (map #(subs % 1))
               (remove (partial contains? jumps))
               distinct)]
    (merge jumps
           (zipmap vars (range BASE 16384)))))

(defn symbol-map
  [instructions]
  (let [jumps (jump-map instructions)
        known (merge jumps base-symbols)]
    (var-map known instructions)))

(defn replace-symbol
  [symbols s]
  {:post [(not (= "@" %))]} ;; indicates a failed lookup
  (if-not (a-var? s)
    s
    (let [sym (subs s 1)
          address (get symbols sym)]
      (str "@" address))))

(def file->lines
  (comp line-seq io/reader io/file)) 

(defn strip-comments
  [lines]
  (->> lines
    (filter code?)
    (map str/trim)))

(defn lines->instructions
  [lines]
  (let [instructions (strip-comments lines)
        table (merge (symbol-map instructions)
                     base-symbols)]
    (->> instructions
      (remove jump?)
      (map (partial replace-symbol table)))))

(defn compile-file*
  "Pure counterpart of compile-file"
  [lines]
  (->> lines
    lines->instructions
    (map compile-instruction)))

(defn compile-file
  [file-in]
  (let [lines (file->lines file-in)
        out-name (str/replace file-in #"\.asm$" ".hack")]
    (->> lines
      compile-file*
      (str/join "\n")
      (spit out-name))))
