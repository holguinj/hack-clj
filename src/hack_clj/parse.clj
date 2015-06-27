(ns hack-clj.parse
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn zfill
  "Returns a string ending with s, padded with zeroes up to n
  characters."
  ([s n] (let [length (- n (count s))
               fill (apply str (repeat length "0"))]
           (str fill s)))
  ([s] (zfill s 15)))

(defn flip-bin-chars
  [bs]
  (->> bs
    (map {\0 \1, \1 0})
    (apply str)))

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

(defn int->binstring
  "Returns a 15-bit twos complement binary string representation of the given int."
  [n]
  {:pre [(integer? n)
         (<= -32767 n 32767)]}
  (if (neg? n)
    (let [complement  (-> (- n)
                        int->binstring
                        flip-bin-chars
                        (Integer/parseInt 2)
                        inc)]
      (int->binstring complement))
    ;; else n >= 0
    (zfill (Integer/toString n 2))))

(defn binstring->int
  [bs]
  {:pre [(= 15 (count bs))]}
  (if (= \1 (first bs)) ;; number is negative
    (-> bs
      flip-bin-chars
      binstring->int
      inc
      (* -1))
    (Integer/parseInt bs 2)))

(defn maybe-parse-int
  "Attempts to parse the given string as an integer. Returns nil if the operation fails."
  [s]
  (when (not-empty s)
    (try (Integer/parseInt s)
         (catch Exception _
           nil))))
