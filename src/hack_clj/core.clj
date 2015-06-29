(ns hack-clj.core
  (:require [hack-clj.parse :as parse]
            [hack-clj.assembler :as asm]
            [hack-clj.vm.analyzer :as vm-analyzer]
            [hack-clj.vm :as vm-gen]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn asm?
  [^java.io.File file]
  (-> file
      (.getName)
      (.endsWith ".asm")))

(defn vm?
  [^java.io.File file]
  (-> file
      (.getName)
      (.endsWith ".vm")))

(defn suffix
  [path]
  (second (re-find #"\.([a-zA-Z]+)$" path)))

(def new-suffixes
  {"vm" "asm"
   "asm" "hack"})

(defn out-path
  [^java.io.File file]
  (let [full-path (.getPath file)
        new-suffix (->> full-path suffix new-suffixes (str "."))]
    (str/replace full-path #"\.([a-zA-Z]+)$" new-suffix)))

(defn program-name
  [^java.io.File file]
  (-> (.getName file)
      (str/replace #"\.[a-zA-Z]+$" "")))

(defn compile!
  [file]
  (let [out (out-path file)
        name (program-name file)
        lines (-> file parse/file->lines parse/strip-comments)]
    (cond
      (vm? file)
      (->> lines
           vm-analyzer/analyze
           (vm-gen/emit-asm name)
           (parse/lines->file out))

      (asm? file)
      (->> lines
           asm/compile
           (parse/lines->file out)))))

(defn -main
  [& args]
  (let [file (io/file (first args))]
    (if (.canRead file)
      (compile! file)
      (println "Failed to read" (first args)))))
