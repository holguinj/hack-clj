(ns hack-clj.util)

(defn re-contains? [^String s re]
  "Returns true if the string contains the pattern."
  (false? (nil? (re-find re s))))

(defn pad [^String s n]
  (if (<= n (.length s))
          s
          (pad (str 0 s) n)))
