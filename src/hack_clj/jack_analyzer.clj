(ns hack-clj.jack-analyzer
  (:require [hack-clj.jack-lexer :as lexer]))


;; IDEA: two functions-- dig-in and back-out
;; they take :paren, :bracket, or :quote
;; each of the three has a mutable stack
;; (maybe not for :quote, but notionally yes)
;; dig-in pushes to the stack when we recurse
;; into a form, then back-out pops from the stack
;; so that it knows which form to close.

(defn token-type
  "Given a line of XML (from the lexer), return
  the token type (i.e., the tag type)."
  [s]
  (->> s
       (re-find #"<([a-z]+)>")
       (second)
       (keyword)))

(defn token-value
  "Given a line of XML (from the lexer), return
  the value between the open and close tags."
  [s]
  (->> s
       (re-find #"<[a-z]+> ([a-z]+) </[a-z]+>")
       (second)
       (keyword)))

(defmulti compile-nonterminal token-value)
