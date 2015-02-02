(ns hack-clj.assembler
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn zfill
  "Returns a string ending with s, padded with zeroes up to n
  characters."
  ([s n] (let [length (- n (count s))
               fill (apply str (repeat length "0"))]
           (str fill s)))
  ([s] (zfill s 15)))

(defn binary
  [n]
  (-> n
    str
    (Integer/parseInt)
    (Integer/toString 2)))

(def extract
  (comp second (partial re-find)))

(def get-dest (partial extract #"(\w+)="))
(def get-jump (partial extract #";(\w+)"))
(defn get-comp [s]
  (let [comp-syms #"([AMD01\!\|\-\&\+]+)"]
    (or (extract (re-pattern (str "=" comp-syms)) s)
        (extract (re-pattern (str comp-syms ";")) s)
        (throw (IllegalArgumentException.
                (str "get-comp called with unparseable input: " s))))))

(defn parse-c-instruction
  [s]
  (let [dest? (re-find #"=" s)
        jump? (re-find #";" s)]
    (merge {:comp (get-comp s)}
           (if dest? {:dest (get-dest s)})
           (if jump? {:jump (get-jump s)}))))

(def comp-instruction
  ;; a=0
  {"0"   "0101010"
   "1"   "0111111"
   "-1"  "0111010"
   "D"   "0001100"
   "A"   "0110000"
   "!D"  "0001101"
   "!A"  "0110001"
   "-D"  "0001111"
   "-A"  "0110011"
   "D+1" "0011111"
   "A+1" "0110111"
   "D-1" "0001110"
   "A-1" "0110010"
   "D+A" "0000010"
   "D-A" "0010011"
   "A-D" "0000111"
   "D&A" "0000000"
   "D|A" "0010101"

   ;; a=1
   "M"   "1110000"
   "!M"  "1110001"
   "-M"  "1110011"
   "M+1" "1110111"
   "M-1" "1110010"
   "D+M" "1000010"
   "D-M" "1010011"
   "D&M" "1000000"
   "D|M" "1010101"})

(def dest-instruction
  {"M"   "001"
   "D"   "010"
   "MD"  "011"
   "A"   "100"
   "AM"  "101"
   "AD"  "110"
   "AMD" "111"})

(def jump-instruction
  {"JGT" "001"
   "JEQ" "010"
   "JGE" "011"
   "JLT" "100"
   "JNE" "101"
   "JLE" "110"
   "JMP" "111"})

(defn generate-c-instruction
  [{:keys [dest comp jump]}]
  (str "111"
       (comp-instruction comp)
       (get dest-instruction dest "000")
       (get jump-instruction jump "000")))

(defn instruction-type
  [s]
  (cond
    (re-find #"@\d+$" s)   :a-instruction
    (re-find #"^.+[=;]" s) :c-instruction
    (re-find #"\(.+\)$" s) :jump-target
    (re-find #"@\w+$" s)   :a-var
    :else (throw (IllegalArgumentException.
                  (str "unknown instruction type " s)))))

(defn assert-output
  [in out]
  (if (and (= 16 (count out))
           (re-matches #"^[01]+$" out))
    out
    (throw (IllegalArgumentException. (str "Compilation failure: " in " -> " out)))))

(defn compile-c-instruction
  "Parse the given string as a C-instruction and return the resulting
  binary string."
  [s]
  (-> s
    parse-c-instruction
    generate-c-instruction))

(defn compile-a-instruction
  [num]
  (let [num-str (str/replace (str num) #"@" "")
        val (binary num-str)]
    (->> val
      zfill
      (str "0"))))

(defn compile-instruction
  [s]
  {:post [(assert-output s %)]}
  (condp = (instruction-type s)
    :a-instruction (compile-a-instruction s)
    :c-instruction (compile-c-instruction s)
    (throw (IllegalArgumentException.
            (str "Can't compile instruction: " s " of type "
                 (instruction-type s) ".")))))

(defn code?
  "Returns true if the given string is not a comment and not blank"
  [s]
  (let [line (-> s (str/replace #"//.*$" "") str/trim)]
    (not (str/blank? line))))

(defn a-var?
  [s]
  (= :a-var
     (instruction-type s)))

(defn jump?
  [s]
  (= :jump-target
     (instruction-type s)))

(defn strip-parens
  [s]
  (str/replace s #"[\(\)]" ""))

(defn jump-map
  [instructions]
  (loop [[inst :as remaining] instructions
         acc {}
         i 0]
    (cond
      (empty? remaining) acc

      (= :jump-target (instruction-type inst))
      (recur (rest remaining)
             (assoc acc (strip-parens inst) i)
             i)

      :else
      (recur (rest remaining)
             acc
             (inc i)))))

(defn var-map
  [jumps instructions]
  (let [BASE 0
        vars (->> instructions
               (filter a-var?)
               (map #(subs % 1))
               (remove (partial contains? jumps)))]
    (merge jumps
           (zipmap vars (range BASE 1024)))))

(defn symbol-map
  [instructions]
  (let [jumps (jump-map instructions)]
    (var-map jumps instructions)))

(defn replace-symbol
  [symbols s]
  (if-not (= :a-var (instruction-type s))
    s
    (let [sym (subs s 1)
          address (get symbols sym)]
      (str "@" address))))

(defn compile-file*
  "Pure counterpart of compile-file"
  [lines]
  (let [instructions (filter code? lines)
        table (symbol-map instructions)]
    (->> instructions
      (remove jump?)
      (map (partial replace-symbol table))
      (map compile-instruction))))

(defn compile-file
  [file-in]
  (let [lines (-> file-in io/file io/reader line-seq)]
    (->> lines
      compile-file*
      (str/join "\n")
      ;; TODO get the real output filename
      (spit "out.hack"))))
