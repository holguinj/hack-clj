(ns hack-clj.util)

(defn re-contains? [^String s re]
  "Returns true if the string contains the pattern."
  (false? (nil? (re-find re s))))

(defn argument [n ^String vm]
  (nth (rest (re-seq #"\S+" vm)) n))

(defn pad [^String s n]
  (if (<= n (.length s))
          s
          (pad (str 0 s) n)))

(def spell-punctuation
  {"(" "open-paren"
   ")" "close-paren"
   "{" "open-brace"
   "}" "close-brace"
   ";" "semicolon"
   "-" "dash"})

(defn keyword
  "Redefining keyword because keywordifying symbols like open-paren
   and semicolon results in BAD BEHAVIOR. I'd rather just translate
   those into strings."
  [name]
  (if-let [special-keyword (spell-punctuation name)]
    (keyword special-keyword)
    (clojure.core/keyword name)))
