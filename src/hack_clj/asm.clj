(ns hack-clj.asm
  (:require [hack-clj.util :refer :all]
            [hack-clj.asm-lookup :refer :all]))

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

(def var-counter (atom 15))

(defn a-instruction?
"Returns true if the input is an A-instruction consisting of '@' followed by one or more digits"
[^String asm]
(false? (nil? (re-find #"^\@[0-9]++$" asm))))

(defn a-var? 
"Returns true if the input is a variable-type A-instruction"
[^String asm]
(false? (nil? (re-find #"^\@[a-zA-Z]++" asm))))

(defn jump? [^String asm]
  (.contains asm ";"))

(defn assignment? [^String asm]
  (.contains asm "="))

(defn target? [^String asm]
  (false? (nil? (re-find #"^\(.+\)$" asm))))

(defn varify! 
  "Given an A-instruction, ensures that a variable is present in the var-table. 
  If there is not, increments var-counter and adds the variable to the table with that value"
  ([^String asm address]
    (let [varname (clojure.string/replace asm #"[\@\(\)]" "")]
      (if (@var-table varname)
          (str "@" (@var-table varname))
          (swap! var-table assoc varname address))))
  ([^String asm] 
    (let [varname (clojure.string/replace asm #"[\@\(\)]" "")]
     (if (@var-table varname)
         (varify! asm (@var-table varname))
         (varify! asm (swap! var-counter inc))))))

(defn get-dest
  "Given a C-instruction, returns the binary segment corresponding to the 'dest' segment, which is the part
  preceding the '='. The 'dest' segment is optional, and this function returns '000' if there is no '='."
  [^String asm]
    (-> (re-find #"^([A-Z]++)=" asm)
        (nth 1)
        (lookup-dest)))

(defn get-comp 
  "Given a C-instruction, returns the binary segment corresponding to the 'comp' segment, 
  which either follows '=' or preceeds ';'. The 'comp' segment is required for C-instructions,
  and this function will produce incorrect output if none is present."
  [^String asm]
  {:pre [(or (jump? asm) (assignment? asm))]
   :post [(= 7 (.length %))]}
    (-> (re-find #"[ADM]*=*([A-Z0-9\-\+\!\&\|\.]++);*" asm) 
        (nth 1)
        (lookup-comp)))

(defn get-jump
  "Given a C-instruction, returns the binary segment corresponding to the 'jump' segment, which
  follows the ';'. The 'jump' segment is optional, and this function will return '000' if it is not
  present."
  [^String asm]
  {:post [(= 3 (.length %))]}
    (-> (re-find #";([A-Z]++)$" asm)
        (nth 1)
        (lookup-jump)))

(defn compile-a-instruction 
  "Given an A-instruction, resolves the variable (if present), strips the leading '@', and returns
  a string containing the 16-digit binary representation of the remaining integer."
  [^String asm]
  (if (a-var? asm) 
      (compile-a-instruction (varify! asm))
      (-> asm
          (subs 1)
          (Integer/parseInt)
          (Integer/toString 2)
          (pad 16))))

(defn compile-c-instruction 
  "Given a C-instruction, combines '111' (indicates C-expression) and the binary corresponding to 
  the 'comp', 'dest', and 'jump' segments."
  [^String asm]
  {:post [(= 16 (.length %))]}
  (str "111"
       (get-comp asm)
       (get-dest asm)
       (get-jump asm)))

(defn cleanup 
  "Given a program line, removes whitespace and comments, then uppercases the result."
  [^String asm]
  (-> asm
      (clojure.string/replace #"\s" "")
      (clojure.string/replace #"\s*//\S*" "")))

(defn hack-compile
  "Given a line of pure assembly code, compiles it as either an A-instruction or a C-instruction"
  [^String asm]
  {:pre [(false? (clojure.string/blank? asm))]
   :post [(= 16 (.length %))]}
  (if (or (a-instruction? asm) (a-var? asm)) 
      (compile-a-instruction asm)
      (compile-c-instruction asm)))

(defn parse-vars 
  "Should be executed before compilation to build the var-table"
  [code]
  (let [line-number (atom 0)]
   (doall 
     (for [line code] 
       (if (target? line) 
           (do 
             (println "Loop target:" line "at" @line-number)
             (varify! line @line-number))
           (swap! line-number inc))))
   (doall 
     (for [line code]
       (if (a-var? line)
           (do (varify! line)))))))

(defn compile-asm
  "Given a seq of pre-processed lines of assembly and a filename, compile the code to
  binary and write it out with that filename."
  [code fout]
  (println "Making initial pass to scan for variables and jump targets.")
  (parse-vars code)
  (spit fout 
    (->> code
      (map cleanup)
      (filter (complement clojure.string/blank?))
      (filter (complement target?))
      (map hack-compile)
      (clojure.string/join "\n"))))
