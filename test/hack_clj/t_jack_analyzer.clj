(ns hack-clj.t-jack-analyzer
  (:use [clojure.test]
        [midje.sweet]
        [hack-clj.jack-analyzer]))

;(facts "about parsing " )

(comment (facts "about splitting input"
  (fact "split-input returns a cons"
    (class (split-input "var input;"))
    => clojure.lang.Cons)
  (fact "split-input splits on keywords, identifiers, and symbols"
    (split-input "var input;")
    => '("var" "input" ";"))))
