(ns hack-clj.assembler)

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

;; @value = A<-value
(defn compile-a-instruction
  [num]
  (let [val (binary num)]
    (->> val
      zfill
      (str "0"))))

;; c-instructions have the following form:
;;   dest=comp;jump
;; where either the dest or jump fields may be empty.
;; if dest is empty, "=" is omitted
;; if jump is empty, ";" is omitted

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

(defn compile-c-instruction
  [{:keys [dest comp jump]}]
  (str "111"
       (comp-instruction comp)
       (get dest-instruction dest "000")
       (get jump-instruction jump "000")))
