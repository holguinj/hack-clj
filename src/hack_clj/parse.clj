(ns hack-clj.parse
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def extract
  (comp second (partial re-find)))

(defn code?
  "Returns true if the given string is not a comment and not blank"
  [s]
  (let [line (-> s (str/replace #"//.*$" "") str/trim)]
    (not (str/blank? line))))

(def file->lines
  (comp line-seq io/reader io/file)) 

(defn strip-comments
  [lines]
  (->> lines
    (filter code?)
    (map str/trim)))
