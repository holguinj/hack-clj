(ns hack-clj.jack
  (:require [instaparse.core :as insta]))

(def jack-parser
  (insta/parser
    "resources/jack.bnf"))

(def transform-options
  {:Name str})

(defn jack-eval [s]
  (->> s
       (jack-parser)
       (insta/transform transform-options)))

(defn compile-file [filename]
  (-> (slurp filename)
      (clojure.string/replace "\n" " ")
      (jack-eval)
      (println)))
