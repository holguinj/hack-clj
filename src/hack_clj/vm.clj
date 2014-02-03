(ns hack-clj.vm
  (:require [hack-clj.vm-lookup :refer :all]
            [hack-clj.util :refer :all]
            [hack-clj.regex :as re]))

(defn command-type [^String vm]
  "Given a command in vm language, returns the type of command:
  :c-arithmetic, :c-push, :c-pop, :c-label, :c-goto, :c-if, :c-function,
  :c-return, or :c-call"
  (cond
    (re-contains? vm re/c-arithmetic) :c-arithmetic
    (re-contains? vm re/c-push) :c-push
    (re-contains? vm re/c-pop) :c-pop
    (re-contains? vm re/c-label) :c-label 
    (re-contains? vm re/c-function) :c-function
    (re-contains? vm re/c-return) :c-return
    (re-contains? vm re/c-call) :c-call))
