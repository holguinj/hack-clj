(ns hack-clj.vm-lookup
  (:require [hack-clj.util :refer :all]))

(def loop-counter (atom 0))
(def ret-counter (atom 0))

(def base-pointer
  {"local" 1
   "argument" 2
   "pointer" 3
   "this" 3
   "that" 4
   "temp" 5
   "static" 16})

(defn push-address [address]
  [(str "@" address)
   "D=M"
   "@SP"
   "A=M"
   "M=D"
   "@SP"
   "M=M+1"])

(defn pop-address [address]
  ["@SP"
   "M=M-1"
   "A=M"
   "D=M"
   (str "@" address)
   "M=D"])

(defmulti compile-push (partial argument 0))

(defmulti compile-pop (partial argument 0))

(defmethod compile-push "constant" [^String vm]
  (let [value (argument 1 vm)]
    [(str "//push constant " value)
     (str "@" value)
     "D=A"
     "@SP"
     "A=M"
     "M=D"
     "@SP"
     "M=M+1"]))

(defmethod compile-push "temp" [^String vm]
  (let [base (base-pointer "temp")
        offset (Integer. (argument 1 vm))
        address (+ base offset)]
    (push-address address)))

(defmethod compile-pop "temp" [^String vm]
  (let [base (base-pointer "temp")
        offset (Integer. (argument 1 vm))
        address (+ base offset)]
    (pop-address address)))

(defmethod compile-push "pointer" [^String vm]
  (let [base (base-pointer "pointer")
        offset (Integer. (argument 1 vm))
        address (+ base offset)]
    (push-address address)))

(defmethod compile-pop "pointer" [^String vm]
  (let [base (base-pointer "pointer")
        offset (Integer. (argument 1 vm))
        address (+ base offset)]
    (pop-address address)))

(defmethod compile-push :default [^String vm]
  (let [segment (argument 0 vm)
        offset (Integer. (argument 1 vm))
        base (base-pointer segment)]
      [(str "//push " segment " " offset)
       (str "@" offset)
       "D=A"
       (str "@" base)
       "A=M"
       "A=A+D"
       "D=M"
       "@SP"
       "A=M"
       "M=D"
       "@SP"
       "M=M+1"]))

(defmethod compile-pop :default [^String vm]
  (let [segment (argument 0 vm)
        offset (Integer. (argument 1 vm))
        base (base-pointer segment)]
      [(str "//pop " segment " " offset)
       "@SP"
       "M=M-1"
       (str "@" base)
       "D=M"
       (str "@" offset)
       "D=D+A"
       "@R13"
       "M=D"
       "@SP"
       "A=M"
       "D=M"
       "@R13"
       "A=M"
       "M=D"]))

(def init 
  '("//Init"
    "@256"
    "D=A"
    "@SP"
    "M=D"))

(def add
  '("//add"
    "@SP"
    "M=M-1"
    "A=M"
    "D=M"
    "M=0"
    "@SP"
    "M=M-1"
    "A=M"
    "M=M+D"
    "@SP"
    "M=M+1"))

(def sub
  '("//sub"
    "@SP"
    "M=M-1"
    "A=M"
    "D=M"
    "M=0"
    "@SP"
    "M=M-1"
    "A=M"
    "M=M-D"
    "@SP"
    "M=M+1"))

(def c-and
  '("//and"
    "@SP"
    "M=M-1"
    "A=M"
    "D=M"
    "@SP"
    "M=M-1"
    "A=M"
    "M=M&D"
    "@SP"
    "M=M+1"))

(def neg
  '("//neg"
    "M=M-1"
    "A=M"
    "M=-M"
    "@SP"
    "M=M+1"))

(def c-not
  '("//not"
    "@SP"
    "M=M-1"
    "A=M"
    "M=!M"
    "@SP"
    "M=M+1"))

(def c-or
  '("//or"
    "@SP"
    "M=M-1"
    "A=M"
    "D=M"
    "@SP"
    "M=M-1"
    "A=M"
    "M=M|D"
    "@SP"
    "M=M+1"))

(defn eq! []
  (swap! loop-counter inc)
  ["//eq"
   "@SP"
   "M=M-1"
   "A=M"
   "D=M"
   "@SP"
   "M=M-1"
   "A=M"
   "M=M-D"
   "D=M"
   "M=0"
   (str "@eq" @loop-counter)
   "D;JNE"
   "@SP"
   "A=M"
   "M=-1"
   (str "(eq" @loop-counter ")")
   "@SP"
   "M=M+1"])

(defn gt! []
  (swap! loop-counter inc)
  ["//gt"
   "@SP"
   "M=M-1"
   "A=M"
   "D=M"
   "@SP"
   "M=M-1"
   "A=M"
   "M=M-D"
   "D=M"
   "M=0"
   (str "@gt" @loop-counter)
   "D;JLE"
   "@SP"
   "A=M"
   "M=-1"
   (str "(gt" @loop-counter ")")
   "@SP"
   "M=M+1"])

(defn lt! []
  (swap! loop-counter inc)
  ["//lt"
   "@SP"
   "M=M-1"
   "A=M"
   "D=M"
   "@SP"
   "M=M-1"
   "A=M"
   "M=M-D"
   "D=M"
   "M=0"
   (str "@lt" @loop-counter)
   "D;JGE"
   "@SP"
   "A=M"
   "M=-1"
   (str "(lt" @loop-counter ")")
   "@SP"
   "M=M+1"])

(defn label [^String vm]
  (let [target (argument 0 vm)]
    [(str "//label " target)
     (str "(" target ")")]))

(defn goto [^String vm]
  (let [target (argument 0 vm)]
    [(str "//goto " target)
     (str "@" target)
     "0;JMP"]))

(defn if-goto [^String vm]
  (let [target (argument 0 vm)]
    [(str "//if-goto " target)
     "@SP"
     "M=M-1"
     "A=M"
     "D=M"
     (str "@" target)
     "D;JNE"]))

(defn push-return []
  "Push a new return address onto the stack.
  Used for function calls."
  (let [ret-addr (str "r" (swap! ret-counter inc))]
    (conj (push-address ret-addr)
          (str "(" ret-addr ")"))))

(defn push-pointers []
  "Pushes LCL, ARG, THIS, and THAT to the stack,
  effectively saving the state of the current frame."
  (flatten 
    (conj 
      (push-address "LCL")
      (push-address "ARG")
      (push-address "THIS")
      (push-address "THAT"))))

(defn init-ARG [arity]
  "Initialize the ARG pointer at the beginning of a function.
  Equivalent to `ARG = SP - arity - 5`."
  (flatten
    (conj
      (push-address "SP")
      (push-address (str arity))
      sub
      (push-address "5")
      sub)))

(defn init-LCL []
  "Initializes the LCL pointer at the beginning of a function.
  Equivalent to `LCL = SP`."
  (flatten
    (conj 
      (push-address "SP")
      (pop-address "LCL"))))
