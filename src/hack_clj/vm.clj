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
  (flattenv
   ["@256"
    "D=A"
    "@SP"
    "M=D"
    prog]))

(defn a-load
  [n]
  (str "@" n))

(defn d-load
  [n]
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
  (str "(" s ")"))

(defn eq
  []
  (flattenv
   (let [ret-true (gensym "eq-true-")
         end (gensym "eq-end-")]
     [sub
      pop-d
      ;; D=0 iff x=y
      (a-load ret-true)
      "D;JEQ"

      ;; skipped if x = y
      (d-load FALSE)
      push-d
      (do-jump end)

      (label ret-true)
      (d-load TRUE)
      push-d

      (label end)])))
