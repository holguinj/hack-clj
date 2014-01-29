(ns hack-clj.util)

(defn pad [^String s n]
  (if (<= n (.length s))
          s
          (pad (str 0 s) n)))
