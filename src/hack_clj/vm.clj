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
    (re-contains? vm re/c-call) :c-call
    :else (println "No matching command type for:" vm)))

(defn argument [n ^String vm]
  (nth (rest (re-seq #"\w+" vm)) n))

(defn compile-arithmetic [^String vm]
  {:pre [(= :c-arithmetic (command-type vm))]}
  (cond
    (re-contains? vm re/c-add) lookup/add 
    (re-contains? vm re/c-sub) lookup/sub
    (re-contains? vm re/c-neg) lookup/neg
    (re-contains? vm re/c-eq) (lookup/eq!)
    (re-contains? vm re/c-lt) (lookup/lt!)
    (re-contains? vm re/c-gt) (lookup/gt!)
    (re-contains? vm re/c-and) lookup/and
    (re-contains? vm re/c-or) lookup/or
    (re-contains? vm re/c-not) lookup/not))

(defn compile-push-constant [^String vm]
  (let [value (argument 1 vm)]
    (lookup/push-constant value)))

(defn compile-push [^String vm]
  {:pre [(= :c-push (command-type vm))]}
  (cond
    (re-contains? vm re/c-push-constant) (compile-push-constant vm)))

(defn compile-instruction
  "Given a line of pure vm code, return a seq of assembly commands"
  [^String vm]
  (clojure.string/join "\n" 
    (case (command-type vm)
      :c-arithmetic (compile-arithmetic vm)
      :c-push (compile-push vm)
      :else (println "No matching command type for:" vm))))

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
       (clojure.string/join "\n"))))
