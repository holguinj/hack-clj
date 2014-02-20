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
    (re-contains? vm re/c-goto) :c-goto 
    (re-contains? vm re/c-if) :c-if
    (re-contains? vm re/c-function) :c-function
    (re-contains? vm re/c-return) :c-return
    (re-contains? vm re/c-call) :c-call
    :else (println "No matching command type for:" vm)))

(defmulti compile-instruction command-type)

(defmethod compile-instruction :c-arithmetic [^String vm]
  (cond
    (re-contains? vm re/c-add) lookup/add 
    (re-contains? vm re/c-sub) lookup/sub
    (re-contains? vm re/c-neg) lookup/neg
    (re-contains? vm re/c-eq) (lookup/eq!)
    (re-contains? vm re/c-lt) (lookup/lt!)
    (re-contains? vm re/c-gt) (lookup/gt!)
    (re-contains? vm re/c-and) lookup/c-and
    (re-contains? vm re/c-or) lookup/c-or
    (re-contains? vm re/c-not) lookup/c-not))

(defmethod compile-instruction :c-push [^String vm]
  (lookup/compile-push vm))

(defmethod compile-instruction :c-pop [^String vm]
  (lookup/compile-pop vm))

(defmethod compile-instruction :c-label [^String vm]
  (lookup/label vm))

(defmethod compile-instruction :c-goto [^String vm]
  (lookup/goto vm))

(defmethod compile-instruction :c-if [^String vm]
  (lookup/if-goto vm))

(defmethod compile-instruction :c-function [^String vm]
  (lookup/function vm))

(defmethod compile-instruction :c-return [_]
  (lookup/return))

(defn vm-cleanup [^String vm]
  (clojure.string/replace vm #"\s*//.*" ""))

(defn compile-vm
  [^String code fout]
  (spit fout
    (->> code
       (map vm-cleanup)
       (filter (complement clojure.string/blank?))
       (map clojure.string/lower-case)
       (map compile-instruction)
       (map (partial clojure.string/join "\n"))
       (clojure.string/join "\n"))))
