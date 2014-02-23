(ns hack-clj.core
  (:gen-class :main true)
  (:require [hack-clj.asm :refer :all]
            [hack-clj.vm :refer :all]
            [hack-clj.util :refer :all]
            [hack-clj.vm-lookup :as lookup]
            [clojure.java.io :as io]))

(defn fout-name 
  "Given a filename from the command line, replace '.vm' with '.asm'
  or replace '.asm' with '.hack'."
  [^String fin]
  (cond 
    (.contains fin ".asm") (clojure.string/replace fin ".asm" ".hack")
    (.contains fin ".vm") (clojure.string/replace fin ".vm" ".asm")))

(defn compile-file
  "Reads a filename from standard input and compiles that file."
  [^String file]
  {:pre [(or (.contains file ".asm")
             (.contains file ".vm"))]}
  (with-open [rdr (io/reader file)]
    (let [fout (fout-name file)
          code (line-seq rdr)]
      (println "Compiling" file "->" fout)
      (if (.contains file ".asm") 
        (compile-asm code fout)
        (compile-vm code fout))))
  (println "Done!"))

(defn qualify-directory [^String directory]
  (if (re-contains? directory #"^/")
    directory
    (str (System/getProperty "user.dir") "/" directory)))

(defn vm-file? [^java.io.File file]
  (re-contains? (.getName file) #"\.vm$"))

(defn compile-directory [^String directory]
  (let [full-path (qualify-directory directory)
        rdr (io/file full-path)
        program-name (last (clojure.string/split full-path #"/"))
        fout (str full-path "/" program-name ".asm")]
    (println "Compiling directory" full-path)
    (spit fout 
          (str (clojure.string/join "\n" lookup/init)
               "\n"
               (->> (file-seq rdr)
                    (filter vm-file?)
                    (map file->asm)
                    (clojure.string/join "\n"))))))

(defn -main [^String input & args]
  (cond
    (.contains input ".asm") (compile-file input)
    (.contains input ".vm") (compile-file input)
    :else (compile-directory input)))
