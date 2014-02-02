(ns hack-clj.core
  (:gen-class :main true)
  (:require [hack-clj.asm :refer :all]
            [clojure.java.io :as io]))

(defn -main 
  "Reads a filename from standard input and compiles that file."
  [^String file & args]
  {:pre [(.contains file ".asm")]}
  (with-open [rdr (io/reader file)]
    (let [fout (clojure.string/replace file ".asm" ".hack")
          code (->> (line-seq rdr)
                    (map cleanup)
                    (filter (complement clojure.string/blank?)))]
      (println "Making initial pass on" file "to scan for variables and jump targets.")
      (parse-vars code)
      (println "Compiling" file "->" fout)
      (spit fout 
        (->> code
             (filter (complement target?))
             (map hack-compile)
             (clojure.string/join "\n")))))
  (println "Done!"))
