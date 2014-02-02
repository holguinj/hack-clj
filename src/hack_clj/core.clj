(ns hack-clj.core
  (:gen-class :main true)
  (:require [hack-clj.asm :refer :all]
            [clojure.java.io :as io]))

(defn fout-name 
  "Given a filename from the command line, replace '.vm' with '.asm'
  or replace '.asm' with '.hack'."
  [^String fin]
  (cond 
    (.contains fin ".asm") (clojure.string/replace fin ".asm" ".hack")
    (.contains fin ".vm") (clojure.string/replace fin ".vm" ".asm")))

(defn -main 
  "Reads a filename from standard input and compiles that file."
  [^String file & args]
  {:pre [(or (.contains file ".asm")
             (.contains file ".vm"))]}
  (with-open [rdr (io/reader file)]
    (let [fout (fout-name file)
          code (->> (line-seq rdr)
                    (map cleanup)
                    (filter (complement clojure.string/blank?)))]
      (println "Compiling" file "->" fout)
      (if (.contains file ".asm") (compile-asm code fout))))
  (println "Done!"))
