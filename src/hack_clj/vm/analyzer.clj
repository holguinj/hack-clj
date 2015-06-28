(ns hack-clj.vm.analyzer
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hack-clj.parse :as parse]))

(defn split-on-space
  [s]
  (str/split s #"\s+"))

(def arithmetic-ops #{"add" "sub" "neg"
                      "eq" "lt" "gt"
                      "and" "or" "not"})

(defn analyze-arithmetic
  [op]
  (if (contains? arithmetic-ops op)
    [:arithmetic op]
    (throw (IllegalArgumentException.
            (format "'%s' is not a recognized operator." op)))))

(def segments #{"local" "argument" "this" "that"
                "pointer" "temp" "static" "constant"})

(defn analyze-push-pop
  [op segment ?offset]
  (if (and (contains? segments segment)
           (contains? #{"push" "pop"} op)
           (not (and (= "pop" op)
                     (= "constant" segment))))
    [(keyword op) (cond-> {:segment segment}
                    ;; TODO wrap parse-int and fail better
                    ?offset (assoc :offset (Integer/parseInt ?offset)))]
    (throw (IllegalArgumentException.
            (format "Can't apply '%s' to '%s'" op segment)))))

(defn analyze-line
  [s]
  (let [parts (split-on-space s)
        [op arg-1 arg-2] parts]
    (cond
      (= 1 (count parts))
      (analyze-arithmetic op)

      (contains? #{"push" "pop"} op)
      (analyze-push-pop op arg-1 arg-2)

      (= "label" op)
      [:label {:ns "Anonymous", :name arg-1}]

      (= "if-goto" op)
      [:if-goto {:ns "Anonymous", :target arg-1}]

      (= "goto" op)
      [:goto {:ns "Anonymous", :target arg-1}]

      :else
      (throw (IllegalArgumentException. (str op " is not yet implemented."))))))

(defn analyze*
  [lines]
  (->> lines
       (mapv analyze-line)))

(defn analyze-file
  [path]
  (->> path
       io/file
       io/reader
       line-seq
       parse/strip-comments
       (filter parse/code?)
       analyze*))
