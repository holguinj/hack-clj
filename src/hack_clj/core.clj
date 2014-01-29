(ns hack-clj.core
  (:require [hack-clj.util :refer :all]))

(defn a-instruction? [asm]
  (false? (nil? (re-find #"^\@" asm))))

(defn compile-a-instruction [^String asm]
  (-> asm
      (subs 1)
      (Integer/parseInt)
      (Integer/toString 2)
      (pad 16)))

(defn lookup [cinst] 
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
    "M" (lookup "A")
    "!M" (lookup "!A")
    "-M" (lookup "-A")
    "M+1" (lookup "A+1")
    "D+M" (lookup "D+A")
    "D-M" (lookup "D-A")
    "M-D" (lookup "A-D")
    "D&M" (lookup "D&A")
    "D|M" (lookup "D|A")} cinst))
