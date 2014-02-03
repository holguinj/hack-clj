(ns hack-clj.vm
  (:require [hack-clj.asm :as asm]))

(defn command-type [^String vm]
  "Given a command in vm language, returns the type of command:
  :c-arithmetic, :c-push, :c-pop, :c-label, :c-goto, :c-if, :c-function,
  :c-return, or :c-call"

  )
