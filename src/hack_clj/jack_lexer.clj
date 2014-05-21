(ns hack-clj.jack-lexer
  (:require [clojure.string :as s]))

(defn- escape [s]
  (-> s
      (s/replace "&" "&amp;")
      (s/replace "<" "&lt;")
      (s/replace ">" "&gt;")))

(defn- strip-comments
  "Takes a string and deletes all comments.
  Comments are defined as //...\n or /*...*/"
  [s]
  (-> s
      (s/replace #"\/\/.*[$\n\r]+" "")
      (s/replace #"(?s)\/\*.*\*\/" "")))

(def keywords
  #{"class" "constructor" "function" "method" "field" "static"
    "var" "int" "char" "boolean" "void" "true" "false" "null"
    "this" "let" "do" "if" "else" "while" "return"})

(def symbols
  #{"{" "}" "(" ")" "[" "]" "." "," ";" "+" "_" "*" "/" "&" "|" "\""
    "<" ">" "=" "-"})

(defn jack-keyword? [w]
  (boolean (keywords w)))

(defn jack-symbol? [c]
  (boolean (symbols c)))

(defn jack-integer? [w]
  (boolean (re-matches #"^[0-9]{1,5}$" w)))

(defn jack-string? [w]
  (boolean (re-matches #"^[^\"\n]*$" w)))

(defn jack-identifier? [w]
  (boolean (re-matches #"^[0-9a-zA-Z_]+$" w)))

(defn token-type [w]
  (cond
    (jack-keyword? w) "keyword"
    (jack-symbol? w) "symbol"
    (jack-integer? w) "integerConstant"
    (jack-identifier? w) "identifier"
    (jack-string? w) "stringConstant"))

(defn split-input
  "This uses some gnarly regex to split the input string into tokens."
  [s]
  (re-seq
   #"[\{\}\(\)\[\]\.\,;\+_\*\/\&\|<>\=\-\"]|[0-9a-zA-Z_]+"
   s))


(defn xml-string
  "Identifies the token type of w and wraps it in an XML entity."
  [w]
  (let [tag (token-type w)]
    (str "<" tag "> "
         (escape w)
         " </" tag ">")))

(defn tokenize [filename]
  (let [fin (-> filename (slurp) (strip-comments) (split-input))
        fout-name (s/replace filename ".jack" ".xml")]
    (spit fout-name
          (str "<tokens>\n"
               (s/join "\n" (map xml-string fin))
               "\n</tokens>\n"))))
