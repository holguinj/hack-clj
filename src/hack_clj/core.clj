(ns hack-clj.core
  (:require [hack-clj.util :refer :all]))

(def var-table (atom {"R0" 0 "SP" 0
                      "R1" 1 "LCL" 1
                      "R2" 2 "ARG" 2
                      "R3" 3 "THIS" 3
                      "R4" 4 "THAT" 4
                      "R5" 5 "R6" 6
                      "R7" 7 "R8" 8
                      "R9" 9 "R10" 10
                      "R11" 11 "R12" 12
                      "R13" 13 "R14" 14
                      "R15" 15 "SCREEN" 16384 
                      "KBD" 24576}))

(defn a-instruction? [^String asm]
  (false? (nil? (re-find #"^\@[0-9]++$" asm))))

(defn a-var? [^String asm]
  (false? (nil? (re-find #"^\@[a-zA-Z]++" asm))))

(defn jump? [^String asm]
  (.contains asm ";"))

(defn assignment? [^String asm]
  (.contains asm "="))

(defn target? [^String asm]
  (false? (nil? (re-find #"^\(.+\)$" asm))))

(defn compile-a-instruction [^String asm]
  (-> asm
      (subs 1)
      (Integer/parseInt)
      (Integer/toString 2)
      (pad 16)))




(defn lookup-comp [c-comp]
  ({"0" "101010"
    "1" "111111"
    "-1" "111010"
    "D" "001100"
    "A" "110000"
    "!D" "001101"
    "!A" "110001"
    "-D" "001111"
    "-A" "110011"
    "D+1" "011111"
    "A+1" "110111"
    "D-1" "001110"
    "A-1" "110010"
    "D+A" "000010"
    "D-A" "010011"
    "A-D" "000111"
    "D&A" "000000"
    "D|A" "010101"
    "M" "001100"
    "!M" "001101"
    "-M" "110011"
    "M+1" "110111"
    "D+M" "000010"
    "D-M" "010011"
    "M-D" "000111"
    "D&M" "000000"
    "D|M" "010101"} c-comp))
