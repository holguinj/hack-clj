(ns hack-clj.core
  (:gen-class :main true)
  (:require [hack-clj.util :refer :all]
            [hack-clj.lookup :refer :all]))

(def var-table (atom {"R0" 0 "SP" 0
                      "R1" 1 "LCL" 1
                      "R2" 2 "ARG" 2
                      "R3" 3 "THIS" 3
                      "R4" 4 "THAT" 4
                      "R5" 5 "R6" 6
                      "R7" 7 "R8" 8
                      "R9" 9 "R10" 10
                      "R11" 11 "R12" 12
                      "R13" 13 "R14" 14
                      "R15" 15 "SCREEN" 16384 
                      "KBD" 24576}))

(defn next-var []
  (->> (vals @var-table)
       (filter #(< % (@var-table "SCREEN")))
       (reduce max)
       (inc)))

(defn a-instruction? [^String asm]
  (false? (nil? (re-find #"^\@[0-9]++$" asm))))

(defn a-var? [^String asm]
  (false? (nil? (re-find #"^\@[a-zA-Z]++" asm))))

(defn jump? [^String asm]
  (.contains asm ";"))

(defn assignment? [^String asm]
  (.contains asm "="))

(defn target? [^String asm]
  (false? (nil? (re-find #"^\(.+\)$" asm))))

(defn code? [^String asm]
  (and (false? (clojure.string/blank? asm))
       (false? (.contains asm "//")))) ; won't worry about comments on a line with code for now

(defn target! [^String asm address]
  (let [varname (clojure.string/replace asm #"[\@\(\)]" "")]
    (swap! var-table assoc varname address)))

(defn varify! [asm]
  (target! asm (next-var)))

(defn get-dest [^String asm]
    (-> (re-find #"^([A-Z]++)=" asm)
        (nth 1)
        (lookup-dest)))

(defn get-comp [^String asm]
    (-> (re-find #"[ADM]*=*([A-Z0-9\-\+\!\&\|]++);*" asm) 
        (nth 1)
        (lookup-comp)))

(defn get-jump [^String asm]
    (-> (re-find #";([A-Z]++)$" asm)
        (nth 1)
        (lookup-jump)))

(defn compile-a-instruction [^String asm]
  (-> asm
      (subs 1)
      (Integer/parseInt)
      (Integer/toString 2)
      (pad 16)))

(defn compile-c-instruction [^String asm]
  (str "111"
       (get-comp asm)
       (get-dest asm)
       (get-jump asm)))

(defn hack-compile [asm]
  (if (a-instruction? asm) (compile-a-instruction asm)
      (compile-c-instruction asm)))

(defn -main [file & args]
  (let [fout (clojure.string/replace file ".asm" ".hack")]
    (println "Compiling" file "->" fout)
    (spit fout 
      (->> (slurp file)
           (clojure.string/split-lines)
           (filter code?)
           (map hack-compile)
           (clojure.string/join "\n"))))
  (println "Done!"))
