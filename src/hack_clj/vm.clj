(ns hack-clj.vm
  (:require [hack-clj.parse :as parse]))

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

(defn push-constant
  [n]
  {:pre [(integer? n)]}
  (flattenv
   [(d-load n) ;; D=n
    push-d]))

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

(defn label
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

        (label ret-true)
        (d-load TRUE)
        push-d

        (label end)]))))

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

(def segment-offsets
  {"local"    1
   "argument" 2
   "this"     3
   "that"     4})

(defn push-segment
  [segment offset]
  {:pre [(>= offset 0)]}
  (flattenv
   (if-let [seg-base (get segment-offsets segment)]
     [(a-load seg-base)
      "A=M"
      "D=A"
      (a-load offset)
      "D=D+A"
      "@13"
      "M=D"
      pop-d
      "@13"
      "A=M"
      "M=D"]
     ;; else, we can't find the offset
     (throw (IllegalArgumentException. (str "'" segment "' is not a known segment."))))))

(defn push-temp
  [offset]
  {:pre [(<= 0 offset 7)]}
  (flattenv
   (let [temp-base 5
         address (+ temp-base offset)]
     [pop-d
      (a-load address)
      "M=D"])))

(defn push
  [{:keys [segment offset]}]
  {:pre [(string? segment)
         (integer? offset)]}
  (flattenv
   (cond
     (= "constant" segment)
     (push-constant offset)

     (= "temp" segment)
     (push-temp offset)

     (contains? segment-offsets segment)
     (push-segment segment offset)

     :else
     (throw (IllegalArgumentException. (str "Can't push to the '" segment "' segment."))))))
