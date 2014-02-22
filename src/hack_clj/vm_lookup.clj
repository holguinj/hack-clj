(ns hack-clj.vm-lookup
  (:require [hack-clj.util :refer :all]))

(def loop-counter (atom 0))
(def ret-counter (atom 0))
(def class-name (atom ""))

(def base-pointer
  {"local" "LCL" 
   "argument" "ARG" 
   "pointer" 3
   "this" "THIS" 
   "that" "THAT"
   "temp" 5
   "static" 16})

(defn push-address [address]
  "Pushes the given value onto the stack.
  Given a variable, pushes the variable's address."
  [(str "//push-address " address)
   (str "@" address)
   "D=A"
   "@SP"
   "A=M"
   "M=D"
   "@SP"
   "M=M+1"])

(defn push-memory [address]
  "Pushes the memory at the given location onto the stack.
  Given a variable, pushes the variable's value."
  [(str "//push-memory " address)
   (str "@" address)
   "D=M"
   "@SP"
   "A=M"
   "M=D"
   "@SP"
   "M=M+1"])

(defn pop-address [address]
  "Pops the top of the stack to the named memory address"
  [(str "//pop-address " address)
   "@SP"
   "M=M-1"
   "A=M"
   "D=M"
   (str "@" address)
   "M=D"])

(defn pop-memory [address]
  "Pops the top of the stack to the memory pointed to by the
  named memory address. Very rarely needed."
  [(str "//pop-memory " address)
   "@SP"
   "M=M-1"
   "A=M"
   "D=M"
   (str "@" address)
   "A=M"
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
  ["@SP"
   "D=M"
   (str "@" arity)
   "D=D-A"
   "@5"
   "D=D-A"
   "@ARG"
   "M=D"])

(defn init-LCL []
  "Initializes the LCL pointer at the beginning of a function.
  Equivalent to `LCL = SP`."
  ["@SP"
   "D=M"
   "@LCL"
   "M=D"])

(defn call [^String vm]
  (let [f (argument 0 vm)
        arity (argument 1 vm)
        ret-addr (str "r" (swap! ret-counter inc))]
    (flatten
      [(str "//" vm)
       "//push return address"
       (push-address ret-addr) ;address because variables aren't real
       "//push pointers"
       (push-pointers)
       (str "//init arg " arity)
       (init-ARG arity)
       "//init LCL"
       (init-LCL)
       (str "//goto " f)
       (goto (str "goto " f))
       (str "(" ret-addr ")")])))

(defn function [^String vm]
  (let [function-name (argument 0 vm)
        locals        (Integer. (argument 1 vm))]
    (flatten 
      [(str "//function " function-name " " locals)
       (str "(" function-name ")")
       (str "//push 0 " locals " times")
       (repeat locals (push-address "0"))])))

(defn return []
  "Return the value on top of the stack and resume execution
  of the calling function."
  (flatten
     ["//return"
      ;FRAME = LCL //FRAME is a temp var 
      "//FRAME = LCL"
      "@LCL"
      "D=M"
      "@FRAME"
      "M=D"
      ;RET = *(FRAME - 5) //put the return ADDRESS in a temp var
      "//RET = *(FRAME - 5)"
      "@FRAME"
      "D=M"
      "@5"
      "D=D-A"
      "A=D"
      "D=M"
      "@RET"
      "M=D"
      ;*ARG = pop() //reposition the return value for the caller
      "//*ARG = pop()"
      "@SP"
      "M=M-1"
      "A=M"
      "D=M"
      "@ARG"
      "A=M"
      "M=D"
      ;SP = ARG+1 //restore the SP of the caller
      "//SP = ARG + 1"
      "@ARG"
      "D=M+1"
      "@SP"
      "M=D"
      ;THAT = *(FRAME-1) //restore THAT of the caller
      "//THAT = *(FRAME - 1)"
      "@FRAME"
      "A=M-1"
      "D=M"
      "@THAT"
      "M=D"
      ;THIS = *(FRAME-2) //restore THIS of the caller
      "//THIS = *(FRAME - 2)"
      "@FRAME"
      "D=M"
      "@2"
      "A=D-A"
      "D=M"
      "@THIS"
      "M=D"
      ;ARG = *(FRAME-3) //restore ARG of the caller
      "//ARG = *(FRAME - 3)"
      "@FRAME"
      "D=M"
      "@3"
      "A=D-A"
      "D=M"
      "@ARG"
      "M=D"
      ;LCL = *(FRAME-4) //restore LCL of the caller
      "//LCL = *(FRAME-4)"
      "@FRAME"
      "D=M"
      "@4"
      "A=D-A"
      "D=M"
      "@LCL"
      "M=D"
      ;goto RET //go to the return address
      "//goto RET"
      "@RET"
      "A=M"
      "0;JMP"]))

(def init 
  (flatten  
    ["//Init"
     "@256"
     "D=A"
     "@SP"
     "M=D"
     "//Set LCL to -1"
     "@0"
     "D=!A"
     "@LCL"
     "M=D"
     "//Set ARG to -1"
     "@0"
     "D=!A"
     "@ARG"
     "M=D"
     "//Set THIS to -1"
     "@0"
     "D=!A"
     "@THIS"
     "M=D"
     "//Set THAT to -1"
     "@0"
     "D=!A"
     "@THAT"
     "M=D"
     "//start Sys.init"
     (call "call Sys.init 0")]))
