(ns hack-clj.vm
  (:require [hack-clj.vm-lookup :as lookup]
            [hack-clj.util :refer :all]
            [hack-clj.regex :as re]))

(defn command-type 
  "Given a command in vm language, returns the type of command:
  :c-arithmetic, :c-push, :c-pop, :c-label, :c-goto, :c-if, :c-function,
  :c-return, or :c-call"
  [^String vm]
  (cond
    (re-contains? vm re/c-arithmetic) :c-arithmetic
    (re-contains? vm re/c-push) :c-push
    (re-contains? vm re/c-pop) :c-pop
    (re-contains? vm re/c-label) :c-label 
    (re-contains? vm re/c-function) :c-function
    (re-contains? vm re/c-return) :c-return
    (re-contains? vm re/c-call) :c-call))

(defn compile-arithmetic [^String vm]
  {:pre [(= :c-arithmetic (command-type vm))]}
  (cond
    (re-contains? vm c-add) lookup/add 
    (re-contains? vm c-sub) lookup/sub))

(comment (defn c-compile 
  "Given a line of pure vm code, return a seq of assembly commands"
  [^String vm]
  (case (command-type vm)
    :c-arithmetic (compile-arithmetic vm)
    ) 
  ))
