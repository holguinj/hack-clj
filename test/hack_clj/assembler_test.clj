(ns hack-clj.assembler-test
  (:require [clojure.test :refer :all]
            [hack-clj.assembler :refer :all]))

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

(deftest compile-a-instruction-test
  (testing "book example"
    (is (= "0000000000000111"
           (compile-a-instruction 7)
           (compile-a-instruction "7")))))

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

(deftest instruction-type-test
  (is (= :a-instruction
         (instruction-type "@123")
         (instruction-type "@foo")
         (instruction-type "@f123")))
  (is (= :c-instruction
         (instruction-type "M+1;jump")
         (instruction-type "dest=D&M")
         (instruction-type "comp=dest;jump")))
  (is (= :jump-target
         (instruction-type "(LOOP)")
         (instruction-type "(1_infinite_loop)"))))

(deftest code?-test
  (is (code? "@fake // inline comment "))
  (is (code? " D=MD"))
  (is (not (code? "//I'm just a comment")))
  (is (not (code? " "))))
