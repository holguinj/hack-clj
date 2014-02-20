(ns hack-clj.vm-lookup
  (:require [hack-clj.util :refer :all]))

(def loop-counter (atom 0))
(def ret-counter (atom 0))
(def class-name (atom ""))

(def base-pointer
  {"local" 1
   "argument" 2
   "pointer" 3
   "this" 3
   "that" 4
   "temp" 5
   "static" 16})

(defn push-address [address]
  "Pushes the given value onto the stack.
  Given a variable, pushes the variable's address."
  [(str "@" address)
   "D=A"
   "@SP"
   "A=M"
   "M=D"
   "@SP"
   "M=M+1"])

(defn push-memory [address]
  "Pushes the memory at the given location onto the stack.
  Given a variable, pushes the variable's value."
  [(str "@" address)
   "D=M"
   "@SP"
   "A=M"
   "M=D"
   "@SP"
   "M=M+1"])

(defn pop-address [address]
  "Pops the top of the stack to the named memory address"
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
    (push-memory address)))

(defmethod compile-pop "temp" [^String vm]
  (let [base (base-pointer "temp")
        offset (Integer. (argument 1 vm))
        address (+ base offset)]
    (pop-address address)))

(defmethod compile-push "pointer" [^String vm]
  (let [base (base-pointer "pointer")
        offset (Integer. (argument 1 vm))
        address (+ base offset)]
    (push-memory address)))

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
    "M=D"
    "//kind of guessing here"
    "@300"
    "D=A"
    "@LCL"
    "M=D"
    "@400"
    "D=A"
    "@ARG"
    "M=D"
    "//start Sys.init"
    "@Sys.init"
    "0;JMP"))

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

(defn push-pointers []
  "Pushes LCL, ARG, THIS, and THAT to the stack,
  effectively saving the state of the current frame."
  (flatten 
    [(push-memory "LCL")
     (push-memory "ARG")
     (push-memory "THIS")
     (push-memory "THAT")]))

(defn init-ARG [arity]
  "Initialize the ARG pointer at the beginning of a function.
  Equivalent to `ARG = SP - arity - 5`."
  (flatten
    [(push-memory "SP")
     (push-address (str arity))
     sub
     (push-address "5")
     sub]))

(defn init-LCL []
  "Initializes the LCL pointer at the beginning of a function.
  Equivalent to `LCL = SP`."
  (flatten
    [(push-memory "SP")
     (pop-address "LCL")]))

(defn call [^String vm]
  (let [f (argument 0 vm)
        arity (argument 1 vm)
        ret-addr (str "r" (swap! ret-counter inc))]
    (flatten
      [(str "//" vm)
       (push-address ret-addr)
       (push-pointers)
       (init-ARG arity)
       (init-LCL)
       (goto (str "goto " f))
       (str "(" ret-addr ")")])))

(defn function [^String vm]
  (let [function-name (argument 0 vm)
        arity         (argument 1 vm)]
    ;set the static segment (unimplemented)
    [(str "(" function-name ")")]))

(defn return []
  "Return the value on top of the stack and resume execution
  of the calling function."
  (flatten
    [;FRAME = LCL //FRAME is a temp var
     (push-address "FRAME")
     (pop-address "LCL")
     ;RET = *(FRAME - 5) //put the return address in a temp var
     (push-address "FRAME") 
     (push-address "5")
     sub
     (pop-address "RET")
     ;*ARG = pop() //reposition the return value for the caller
     (pop-address "ARG")
     ;SP = ARG+1 //restore the SP of the caller
     (push-address "ARG")
     (push-address "1")
     add
     (pop-address "SP")
     ;THAT = *(FRAME-1) //restore THAT of the caller
     (push-address "FRAME")
     (push-address "1")
     sub
     (pop-address "THAT")
     ;THIS = *(FRAME-2) //restore THIS of the caller
     (push-address "FRAME")
     (push-address "2")
     sub
     (pop-address "THIS")
     ;ARG = *(FRAME-3) //restore ARG of the caller
     (push-address "FRAME")
     (push-address "3")
     sub
     (pop-address "ARG")
     ;LCL = *(FRAME-4) //restore LCL of the caller
     (push-address "FRAME")
     (push-address "4")
     sub
     (pop-address "LCL")
     ;goto RET //go to the return address
     (goto "goto RET")]))
