(ns hack-clj.assembler-test
  (:require [clojure.test :refer :all]
            [hack-clj.assembler :refer :all]
            [clojure.java.io :as io]))

(deftest zfill-test
  (testing "can be called with arity 1"
    (let [res (zfill "x")]
      (is (= "00000000000000x" res))))
  (testing "can be called with arity 2"
    (let [res (zfill "x" 5)]
      (is (= "0000x")))))

(deftest parse-c-instruction-test
  (testing "book examples"
    (is (= (parse-c-instruction "MD=M+1")
           {:dest "MD"
            :comp "M+1"}))
    (is (= (parse-c-instruction "D=D-A")
           {:comp "D-A"
            :dest "D"})))
  (testing "when dest, comp, and jump are present"
    (is (= (parse-c-instruction "dest=-D;jump")
           {:dest "dest"
            :comp "-D"
            :jump "jump"})))
  (testing "when dest is missing"
    (is (= (parse-c-instruction "M+1;jump")
           {:comp "M+1"
            :jump "jump"})))
  (testing "when jump is missing"
    (is (= (parse-c-instruction "dest=D&M")
           {:dest "dest"
            :comp "D&M"}))))

(deftest generate-c-instruction-test
  (testing "book examples"
    (is (= "1111110111011000"
           (generate-c-instruction {:dest "MD"
                                    :comp "M+1"})))
    (is (= "1110010011010000"
           (generate-c-instruction {:dest "D"
                                    :comp "D-A"})))))

(deftest compile-c-instruction-test
  (testing "book examples"
    (is (= "1111110111011000"
           (compile-c-instruction "MD=M+1")))
    (is (= "1110010011010000"
           (compile-c-instruction "D=D-A")))))

(deftest compile-a-instruction-test
  (testing "book example"
    (is (= "0000000000000111"
           (compile-a-instruction 7)
           (compile-a-instruction "7")))))

(deftest instruction-type-test
  (is (= :a-instruction
         (instruction-type "@123")))
  (is (= :c-instruction
         (instruction-type "M+1;jump")
         (instruction-type "dest=D&M")
         (instruction-type "comp=dest;jump")))
  (is (= :jump-target
         (instruction-type "(LOOP)")
         (instruction-type "(1_infinite_loop)")))
  (is (= :a-var
         (instruction-type "@f123")
         (instruction-type "@VAR"))))

(deftest code?-test
  (is (code? "@fake // inline comment "))
  (is (code? " D=MD"))
  (is (not (code? "//I'm just a comment")))
  (is (not (code? " "))))

(deftest compile-instruction-test
  (testing "C-instructions"
    (is (= "1111110111011000"
           (compile-instruction "MD=M+1")))
    (is (= "1110010011010000"
           (compile-instruction "D=D-A"))))
  (testing "A-instructions"
    (is (= "0000000000000111"
           (compile-instruction "@7")))))

(deftest jump-map-test
  (is (= {"FOO" 0
          "bar" 3
          "Baz" 5}
         (jump-map ["(FOO)" "@foo" "@bar" "0;JNE" "(bar)"
                    "D=MD" "D=0" "(Baz)"]))))

(deftest var-map-test
  (testing "without jumps"
    (is (= {"foo" 16
            "bar" 17
            "baz" 18}
           (var-map {} ["D=0" "@foo" "M=1" "@bar" "MD=D"
                        "0;jmp" "@baz" "1;JEQ"]))))
  (testing "with jumps"
    (is (= {"quux"  1
            "foo"   16
            "bar"   17
            "baz"   18
            "blorp" 123}
           (var-map {"quux" 1
                     "blorp" 123} ["D=0" "@foo" "M=1" "@bar" "MD=D"
                                   "@quux" "0;jmp" "@baz" "1;JEQ" "@blorp"])))))

(deftest symbol-map-test
  (is (= (merge {"sys.init" 0
                 "bar" 16
                 "baz" 17
                 "quux" 3}
                base-symbols)
         (symbol-map ["(sys.init)" "D=0" "@bar" "@baz" "(quux)" "D=0" "@sys.init" "@bar"]))))

(deftest replace-symbol-test
  (is (= "@12"
         (replace-symbol {"foo" 12}
                         "@foo")
         (replace-symbol {}
                         "@12")))
  (is (= "@1"
         (replace-symbol {"sys.init" 1}
                         "@sys.init"))))

(defn compare-compilation
  [asm-in hack-in]
  (let [input-lines     (-> (str "dev-resources/assembler/" asm-in)  file->lines)
        reference-lines (-> (str "dev-resources/assembler/" hack-in) file->lines)
        output-lines    (compile-file* input-lines)]
    (testing asm-in
      (testing "compiles with the right number of output lines"
        (is (= (count output-lines)
               (count reference-lines))))
      (testing "compiles to bytecode equivalent to the reference assembler's"
        (is (= reference-lines
               output-lines))))))

(deftest ^:acceptance assembler-acceptance-test
  (testing "without symbols"
    (compare-compilation "Add.asm" "Add.hack")
    (compare-compilation "MaxL.asm" "MaxL.hack")
    (compare-compilation "PongL.asm" "PongL.hack")
    (compare-compilation "RectL.asm" "RectL.hack"))
  (testing "built-in symbols"
    (compare-compilation "Registers.asm" "Registers.hack"))
  (testing "jumps and vars"
    (compare-compilation "Labels.asm" "Labels.hack"))
  (testing "reference program"
    (compare-compilation "Max.asm" "Max.hack")
    (compare-compilation "Pong.asm" "Pong.hack")
    (compare-compilation "Rect.asm" "Rect.hack")))
