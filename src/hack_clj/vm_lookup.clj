(ns hack-clj.vm-lookup)

(def init 
  ["//Init"
   "@256"
   "D=A"
   "@SP"
   "M=D"])

(def push
  ["//push above @value to the stack"
   "D=A"
   "@SP"
   "A=M"
   "M=D"
   "@SP"
   "M=M+1"])

(def add
  ["//add"
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
   "M=M+1"])