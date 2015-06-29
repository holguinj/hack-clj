(ns hack-clj.vm
  (:require [hack-clj.parse :as parse])
  (:refer-clojure :exclude [pop]))

(def flattenv (comp vec flatten))

(def ^:const TRUE
  "i.e., 15 ones in binary"
  32767)

(def ^:const FALSE
  "i.e., 15 zeros in binary"
  0)

(defn wrap-init
  [prog]
  {:post [(every? string? %)]}
  (flattenv
   ["@256"
    "D=A"
    "@SP"
    "M=D"
    prog]))

(defn a-load
  [n]
  {:pre [(some? n)]}
  (str "@" n))

(defn d-load
  [n]
  {:pre [(some? n)]}
  [(a-load n)
   "D=A"])

(def push-d
  ["@SP"     ;; A=0
   "A=M"     ;; A=M[0], e.g., A=256
   "M=D"     ;; M[256]=n
   "@SP"     ;; A=0
   "M=M+1"]) ;; SP=SP+1

(def dec-sp
  ["@SP"
   "M=M-1"])

(def inc-sp
  ["@SP"
   "M=M+1"])

(def pop-d
  (flattenv
   [dec-sp
    "@SP"
    "A=M"
    "D=M"]))

(defn with-top-two-from-stack
  "Pops into D, then pops into M, then executes the instructions and increments the pointer.
  You should store the result of this computation in M."
  [& insts]
  {:post [(every? string? %)]}
  (flattenv
   [pop-d   ;; D=stack[0]
    dec-sp
    "@SP"
    "A=M"   ;; M=stack[-1]
    insts
    inc-sp]))

(def add
  (with-top-two-from-stack
    "M=D+M"))

(def sub
  (with-top-two-from-stack
    "M=M-D"))

(def neg
  (flattenv
   [pop-d
    "@SP"
    "A=M"
    "M=-D"
    inc-sp]))

(defn do-jump
  [label]
  {:pre [(some? label)]}
  (flattenv
   [(a-load label)
    "0;JMP"]))

(def pop-d
  (flattenv
   ["@SP"
    "A=M"
    "A=A-1"
    "D=M"
    dec-sp]))

(defn label*
  ;; TODO this function is kinda bogus now
  [s]
  {:pre [(or (string? s)
             (symbol? s))]}
  (str "(" s ")"))

(defn comparison-fn
  "Returns a function that generates code for a vm comparison operator.
  'jump-instruction' should be one of JGT, JGE, JEQ, etc. The 'prefix' will be
  added to the emitted code for clarity."
  [jump-instruction prefix]
  (fn []
    (flattenv
     (let [ret-true (gensym (str prefix "-true-"))
           end (gensym (str prefix "-end-"))]
       [sub
        pop-d
        ;; D=0 iff x=y
        (a-load ret-true)
        (str "D;" jump-instruction)

        ;; skipped if comparison to zero is true
        (d-load FALSE)
        push-d
        (do-jump end)

        (label* ret-true)
        (d-load TRUE)
        push-d

        (label* end)]))))

(def eq (comparison-fn "JEQ" "eq"))

(def gt (comparison-fn "JGT" "gt"))

(def lt (comparison-fn "JLT" "lt"))

(defn boolean-op
  [bool-op]
  (with-top-two-from-stack
    [(str "M=" bool-op)])) ;; e.g,. D|M

(def b-and (boolean-op "D&M"))

(def b-or (boolean-op "D|M"))

(def b-not
  (flattenv
   [pop-d
    "D=!D"
    push-d]))

(defn arithmetic
  [operator]
  (case operator
    "add" add
    "sub" sub
    "neg" neg
    "eq"  (eq)
    "gt"  (gt)
    "lt"  (lt)
    "and" b-and
    "or"  b-or
    "not" b-not
    (throw (IllegalArgumentException.
            (format "'%s' is not a known operator." operator)))))

(def segment-offsets
  {"local"    1
   "argument" 2
   "this"     3
   "that"     4})

(defn pop-segment
  [segment offset]
  {:pre [(>= offset 0)
         (contains? segment-offsets segment)]}
  (flattenv
   (let [seg-base (get segment-offsets segment)
         temp 13]
     [(a-load seg-base)
      "A=M"
      "D=A"
      (a-load offset)
      "D=D+A"
      (a-load temp)
      "M=D"
      pop-d
      (a-load temp)
      "A=M"
      "M=D"])))

(defn pop-temp
  [offset]
  {:pre [(<= 0 offset 7)]}
  (flattenv
   (let [temp-base 5
         address (+ temp-base offset)]
     [pop-d
      (a-load address)
      "M=D"])))

(defn pop-pointer
  [offset]
  {:pre [(contains? #{0 1} offset)]}
  (flattenv
   (let [address (case offset
                   0 "THIS"
                   1 "THAT")]
     [pop-d
      (a-load address)
      "M=D"])))

(defn pop-static
  ([offset] (pop-static "Anonymous" offset))
  ([ns offset]
   {:pre [(string? ns)
          (integer? offset)]}
   (flattenv
    (let [sym (str ns "." offset)]
      [pop-d
       (a-load sym)
       "M=D"]))))

(defn pop
  [{:keys [segment offset]}]
  {:pre [(string? segment)
         (integer? offset)]}
  (flattenv
   (cond
     (= "pointer" segment)
     (pop-pointer offset)

     (= "temp" segment)
     (pop-temp offset)

     (= "static" segment)
     (pop-static offset)

     (contains? segment-offsets segment)
     (pop-segment segment offset)

     :else
     (throw (IllegalArgumentException. (str "Can't pop from the '" segment "' segment."))))))

(defn push-constant
  [n]
  {:pre [(integer? n)]}
  (flattenv
   [(d-load n)
    push-d]))

(defn push-temp
  [offset]
  {:pre [(<= 0 offset 7)]}
  (flattenv
   (let [base 5
         address (+ base offset)]
     [(a-load address)
      "D=M"
      push-d])))

(defn push-static
  ([offset] (push-static "Anonymous" offset))
  ([ns offset]
   {:pre [(string? ns)
          (integer? offset)]}
   (flattenv
    (let [sym (str ns "." offset)]
      [(a-load sym)
       "D=M"
       push-d]))))

(defn push-pointer
  [offset]
  {:pre [(contains? #{0 1} offset)]}
  (flattenv
   (let [address (case offset
                   0 "THIS"
                   1 "THAT")]
     [(a-load address)
      "D=M"
      push-d])))

(defn push-segment
  [segment offset]
  {:pre [(contains? segment-offsets segment)
         (>= offset 0)]}
  (flattenv
   (let [pointer (get segment-offsets segment)
         temp 13]
     [(a-load pointer)
      "D=M"
      (a-load offset)
      "A=D+A"
      "D=M"
      push-d])))

(defn push
  [{:keys [segment offset]}]
  {:pre [(string? segment)
         (integer? 0)]}
  (flattenv
   (cond
     (= "static" segment)
     (push-static offset)

     (= "constant" segment)
     (push-constant offset)

     (= "pointer" segment)
     (push-pointer offset)

     (= "temp" segment)
     (push-temp offset)

     (contains? segment-offsets segment)
     (push-segment segment offset)

     :else
     (throw (IllegalArgumentException. (str "Can't push to the '" segment "' segment."))))))

(defn label
  [{:keys [ns name]}]
  (let [namespace (or ns "Anonymous")]
    (label* (str namespace "." name))))

(defn goto
  [{:keys [ns target]}]
  {:pre [(every? some? [ns target])]}
  (do-jump (str ns "." target)))

(defn if-goto
  [{:keys [ns target]}]
  {:pre [(some? target)]}
  (let [qualified-target (str ns "." target)]
    (flattenv
     [pop-d
      (a-load qualified-target)
      "D;JNE"])))

(defn emit-line
  [[type data]]
  (case type
    :push (push data)
    :pop (pop data)
    :arithmetic (arithmetic data)
    :label (label data)
    :if-goto (if-goto data)
    :goto (goto data)

    (throw (IllegalArgumentException. (str type " is not implemented.")))))

(defn emit-asm
  [name instructions]
  (flattenv
   (mapv emit-line instructions)))
