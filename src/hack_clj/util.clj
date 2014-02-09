(ns hack-clj.util)

(defn re-contains? [^String s re]
  "Returns true if the string contains the pattern."
  (false? (nil? (re-find re s))))

(defn argument [n ^String vm]
  (nth (rest (re-seq #"\w+" vm)) n))

(defn pad [^String s n]
  (if (<= n (.length s))
          s
          (pad (str 0 s) n)))
