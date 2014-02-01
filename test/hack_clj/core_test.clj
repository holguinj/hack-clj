(ns hack-clj.core-test
  (:require [clojure.test :refer :all]
            [hack-clj.lookup :refer :all]
            [hack-clj.core :refer :all]))

(deftest test-a-instruction?
  (testing "should return true if string begins with '@'"
    (is (true? (a-instruction? "@123"))))
  (testing "should return false if non-numbers follow the '@'"
    (is (false? (a-instruction? "@foo")))))

(deftest test-jump?
  (testing "should match iff input contains ';'"
    (is (true? (jump? "0;JGT")))
    (is (false? (jump? "D=!M")))))

(deftest test-assignment?
  (testing "should match iff input contains '='"
    (is (true? (assignment? "D=!M")))
    (is (false? (assignment? "D;!M")))))

(deftest test-a-var?
  (testing "should match if input starts with '@' followed by at least one letter"
    (is (true? (a-var? "@ab12")))
    (is (false? (a-var? "@12ab")))))

(deftest test-target!
  (testing "varify! should give '@FOO' address 16"
    (varify! "@FOO")
    (is (= 16 (@var-table "FOO"))))
  (testing "target! should give '(SEVENS)' address 777"
    (target! "(SEVENS)" 777)
    (is (= 777 (@var-table "SEVENS")))))

(deftest test-compile-a-instruction
  (testing "should return a 16-digit binary number"
    (is (= 16 (.length (compile-a-instruction "@123")))))
  (testing "@123 should come back as 123 in binary"
    (is (= 123 (Integer/parseInt (compile-a-instruction "@123") 2))))) 

(deftest test-target?
  (testing "should match if line looks like (ABC)"
    (is (true? (target? "(LOOP)")))
    (is (false? (target? "@(FOO)")))))

(deftest test-lookup-comp
  (testing "Output for D=M should match reference"
    (is (= "1111110000010000") (lookup-comp "D=M")))
  (testing "Output for M=0 should match reference"
    (is (= "1110101010001000") (lookup-comp "M=0"))))

(deftest test-compile-c-instruction
  (testing "output should match reference"
    (is (= "1111110000010000" (compile-c-instruction "D=M")))
    (is (= "1110001100001000" (compile-c-instruction "M=D")))
    (is (= "1110101010001000" (compile-c-instruction "M=0")))
    (is (= "1110001100000010" (compile-c-instruction "D;JEQ")))
    (is (= "1111110010001000" (compile-c-instruction "M=M-1")))))
