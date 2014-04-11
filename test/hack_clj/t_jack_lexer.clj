(ns hack-clj.t-jack-lexer
  (:use [clojure.test]
        [midje.sweet]
        [hack-clj.jack-lexer]))

(facts "about splitting input"
  (fact "split-input returns a cons"
    (class (split-input "var input;"))
    => clojure.lang.Cons)
  (fact "split-input splits on keywords, identifiers, and symbols"
    (split-input "var input;")
    => '("var" "input" ";")))

(facts "about generating XML"
  (fact "xml-string correctly processes 'var'"
    (xml-string "var") => "<keyword> var </keyword>")
  (fact "xml-string correctly processes 'input'"
    (xml-string "input") => "<identifier> input </identifier>")
  (fact "xml-string correctly processes ';'"
    (xml-string ";") => "<symbol> ; </symbol>"))
