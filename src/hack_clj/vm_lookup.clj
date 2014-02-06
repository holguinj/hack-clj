(ns hack-clj.vm-lookup)

(def loop-counter (atom 0))

(def addresses
  {"constant" 0
   })

(def init 
  '("//Init"
    "@256"
    "D=A"
    "@SP"
    "M=D"))

(def push
  '("//push above @value to the stack"
    "D=A"
    "@SP"
    "A=M"
    "M=D"
    "@SP"
    "M=M+1"))

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

(def and
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

(def not
  '("//not"
    "@SP"
    "M=M-1"
    "A=M"
    "M=!M"
    "@SP"
    "M=M+1"))

(def or
  '("@SP"
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
  ["@SP"
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
  ["@SP"
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
  ["@SP"
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
