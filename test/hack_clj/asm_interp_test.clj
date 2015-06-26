(ns hack-clj.asm-interp-test
  (:require [hack-clj.asm-interp :refer :all]
            [hack-clj.parse :as parse]
            [hack-clj.assembler :as asm]
            [clojure.test :refer :all]))

(def sum-100
  "A program in hack-asm that sums the numbers 1...100"
  ["@i" "M=1" "@sum" "M=0" "(LOOP)" "@i" "D=M" "@100"
   "D=D-A" "@END" "D;JGT" "@i" "D=M" "@sum" "M=D+M"
   "@i" "M=M+1" "@LOOP" "0;JMP" "(END)" "@END" "0;JMP"])

(defn hack->interpretable
  [hack-asm]
  (->> hack-asm
    asm/lines->instructions
    (mapv instruction->interpretable)))

(deftest parse-instruction-test
  (are [code parsed] (= parsed (parse-instruction code))
    "0;JMP" [:c-instruction {:comp "0", :jump "JMP"}]
    "@12"   [:a-instruction 12]
    "@-13"  [:a-instruction -13]
    "D;JGT" [:c-instruction {:comp "D", :jump "JGT"}]
    "D=M"   [:c-instruction {:dest "D", :comp "M"}]
    "M=1"   [:c-instruction {:dest "M", :comp "1"}]
    "M=D+M" [:c-instruction {:dest "M", :comp "D+M"}]))

(deftest instruction->interpretable-test
  (testing "a-instructions"
    (are [code parsed] (= parsed (instruction->interpretable code))
      "@123" [:a-instruction 123]
      "@0"   [:a-instruction 0]
      "@000" [:a-instruction 0]))

  (testing "jumps"
    (are [code parsed] (= parsed (instruction->interpretable code))
      "M;JGE" [:jump {:comp "M", :jump "JGE"}]
      "0;JLT" [:jump {:comp "0", :jump "JLT"}]
      "D;JEQ" [:jump {:comp "D", :jump "JEQ"}]))

  (testing "assignments"
    (are [code parsed] (= parsed (instruction->interpretable code))
      "MD=0"  [:assignment {:dest "MD", :comp "0"}]
      "AMD=1" [:assignment {:dest "AMD", :comp "1"}]
      "A=D|M" [:assignment {:dest "A", :comp "D|M"}]
      "D=!A"  [:assignment {:dest "D", :comp "!A"}])))

(deftest remove-end-loop-test
  (testing "remove end loop"
    (let [interp (hack->interpretable sum-100)]
      (testing "removes the last instruction if appropriate"
        (let [res (remove-end-loop interp)]
          (is (= (count res) (dec (count interp))))))

      (testing "does not remove the last instruction"
        (testing "if the penultimate does not load its own address into A"
          (let [penult (- (count interp) 2)
                dummy (instruction->interpretable "D=0")
                no-loop (assoc interp penult dummy)
                res (remove-end-loop no-loop)]
            (is (= no-loop res))))

        (testing "if the final instruction is not a jump"
          (let [no-jump (butlast interp)
                res (remove-end-loop no-jump)]
            (is (= no-jump res)))))))) 

(def blank-ram {:registers {:A 0, :D 0}})

(deftest register-manipulation-test
  (testing "set-register"
    (testing "sets A"
      (are [val] (= {:registers {:A val, :D 0}} (set-register blank-ram :A val))
        1 -1 3))
    (testing "sets D"
      (are [val] (= {:registers {:A 0, :D val}} (set-register blank-ram :D val))
        1 -1 3))
    (testing "sets M"
      (let [mem (-> blank-ram
                  (set-register :A 42)
                  (set-register :M 666))]
        (is (= 666
               (get mem 42)
               (get-register mem :M))))))

  (testing "get-register"
    (let [mem (-> blank-ram
                (set-register :A 1)
                (set-register :D 4)
                (set-register :M 360))]
      (testing "gets A"
        (is (= 1 (get-register mem :A))))
      (testing "gets D"
        (is (= 4 (get-register mem :D))))
      (testing "gets M"
        (is (= 360
               (get-register mem :M)
               (get mem 1)))))))

(deftest binary-ops-test
  (testing "b-not"
    (testing "is surjective"
      (doseq [i (range -100 100)]
        (is (= i (b-not (b-not i))))))

    (testing "examples"
      (are [in out] (= out (b-not in))
        -13   12
        12    -13
        -1    0
        0     -1)))

  (testing "b-and"
    (are [pair out] (= out (apply b-and pair))
      [12 13]   12
      [13 12]   12
      [0 1]     0
      [1 0]     0
      [0 255]   0
      [255 0]   0
      [666 666] 666
      [100 50]  32
      [50 100]  32))

  (testing "b-or"
    (are [pair out] (= out (apply b-or pair))
      [0 1]   1
      [1 0]   1
      [1 12]  13
      [12 1]  13
      [13 25] 29
      [25 13] 29)))

(deftest compute-test
  (testing "with natural numbers"
    (let [mem (-> blank-ram
                (set-register :A 42)
                (set-register :D 8)
                (set-register :M 666))]
      (are [comp val] (= val (compute mem comp))
        "0"   0
        "A"   42
        "D"   8
        "M"   666
        "M-D" (- 666 8)
        "D-M" (- 8 666)
        "D+M" (+ 666 8)
        "D+1" 9
        "-M"  -666)))
  (testing "including negative numbers"
    (let [mem (-> blank-ram
                (set-register :A -10)
                (set-register :D -4)
                (set-register :M 3))]
      (are [comp val] (= val (compute mem comp))
        "A"   -10
        "D"   -4
        "M"   3
        "M-D" (- 3 -4)
        "D-M" (- -4 3)
        "A-D" (- -10 -4)
        "D-A" (- -4 -10)
        "D+M" (+ -4 3)
        "D+1" -3
        "-M"  -3))))

(deftest jump-test
  (are [comparison+val res] (= res (apply jump? comparison+val))
    ["JGT" 1]  true
    ["JGT" 0]  false
    ["JEQ" 0]  true
    ["JEQ" 9]  false
    ["JGE" -1] false
    ["JGE" 0]  true
    ["JGE" 7]  true
    ["JLT" 9]  false
    ["JLT" -8] true
    ["JNE" 0]  false
    ["JNE" 1]  true
    ["JLE" 0]  true
    ["JLE" -4] true
    ["JLE" 8]  false
    ["JMP" 12] true
    ["JMP" -1] true))

(defn- read-test-program
  [name]
  (->> name
    (str "dev-resources/assembler/")
    parse/file->lines
    parse/strip-comments))

(deftest acceptance-test
  (testing "sum 100"
    (is (= {:vars {"sum" 5050, "i" 101}
            :registers {:A 18, :D 1}
            17 5050
            16 101}
           (run sum-100))))
  (testing "Add.asm"
    (let [add-asm (read-test-program "Add.asm")
          mem (run add-asm)]
      (is (= {"R0" 5}
             (:vars mem)))))
  (testing "Max.asm"
    (let [max-asm (read-test-program "Max.asm")
          input {0 99, 1 30, 2 57, 66 42}
          mem (run max-asm input)]
      (is (= 99 (get mem 0)))
      (testing "with hilarious FFI"
        (let [max-ffi (fn [a b c] (let [input {0 a, 1 b, 2 c}
                                        results (run max-asm input)]
                                    (get results 0)))]
          (are [a+b+c res] (apply max-ffi a+b+c)
            [12 2 45] 45
            [0 1 2] 2
            [2 1 0] 2
            [666 23 888] 888
            [0 0 1] 1
            [32 1 12] 32))))))

(deftest run-states-test
  (testing "sum 100"
    (let [i 16
          sum 17
          states (run-states sum-100)
          final-state (last states)]
      (testing "returns the right result"
        (is (= 5050 (get final-state sum)))
        (is (= 101 (get final-state i))))
      (testing "contains the correct sequence of intermediate states"
        (let [sum-reductions (reductions + (range 101))
              sum-states (->> states
                           (map #(get % sum 0))
                           distinct)
              counter-states (->> states
                               (map #(get % i 0))
                               distinct)]
          (is (= sum-reductions sum-states))
          (is (= (range 102) counter-states)))))))
