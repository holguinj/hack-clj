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
