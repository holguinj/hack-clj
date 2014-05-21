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

(def nonterminal
  #{:class :class-var-dec :subroutine-dec :parameter-list
    :subroutine-body :var-dec :statements :while-statement :if-statement
    :return-statement :let-statement :do-statement})

(defn token-value
  "Given a line of XML (from the lexer), return
  the value between the open and close tags."
  [s]
  (->> s
       (re-find #"<[a-z]+> ([a-z]+) </[a-z]+>")
       (second)
       (keyword)))

(defn new-state
  "Returns a map with blank atomic lists to be used
  as stacks during recursive descent."
  []
  {:paren (atom ())
   :brace (atom ())})

(defn dig-in
  "Given a state map, punctuation (:paren or :brace)
  and the tag-type (e.g., classDec), push the tag onto
  the appropriate stack and return the opening XML for
  the tag."
  [state punctuation tag-type]
  (swap! (state punctuation) conj tag-type)
  (str "<" (name tag-type) ">"))

(defn back-out
  "The inverse of dig-in. Takes a state map and punctuation
  (presumably closing) and pops the tag from the corresponding
  stack. Returns the closing XML for the tag."
  [state punctuation]
  (let [tag-type (first @(state punctuation))]
    (try
      (swap! (state punctuation) rest)
      (catch NullPointerException e
        "back-out called in an invalid state."))
    (str "</" (name tag-type) ">")))

(defmulti compile-nonterminal token-value)
