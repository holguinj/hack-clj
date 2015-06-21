(ns hack-clj.vm
  (:require [hack-clj.parse :as parse]))

(def flattenv (comp vec flatten))

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

(defn with-top-two-from-stack
  "Pops into D, then pops into M, then executes the instructions and increments the pointer.
  You should store the result of this computation in M."
  [& insts]
  (flattenv
   [dec-sp
    "@SP"
    "A=M"
    "D=M"   ;; D=stack[0]
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

