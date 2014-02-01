(ns hack-clj.core
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

(defn varify! [^String asm]
  (let [varname (clojure.string/replace asm #"[\@\(\)]" "")
        address (next-var)]
    (swap! var-table assoc varname address)))

(defn get-comp [^String asm]
    (-> (re-find #"^([A-Z]++)=" asm)
        (nth 1)
        (lookup-comp)))

(defn get-dest [^String asm]
    (-> (re-find #"=([A-Z]++)[;$]" asm)
        (nth 1)
        (lookup-dest)))

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
