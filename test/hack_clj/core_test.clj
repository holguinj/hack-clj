(ns hack-clj.core-test
  (:require [clojure.test :refer :all]
            [hack-clj.core :refer :all]))

(deftest test-a-instruction?
  (testing "should return true if string begins with '@'"
    (is (= true (a-instruction? "@123"))))
  (testing "should return false if non-numbers follow the '@'"
    (is (= false (a-instruction? "@foo")))))

(deftest test-compile-a-instruction
  (testing "should return a 16-digit binary number"
    (is (= (.length (compile-a-instruction "@123")) 16)))
  (testing "@123 should come back as 123 in binary"
    (is (= 123 (Integer/parseInt (compile-a-instruction "@123") 2)))))

(deftest test-lookup-comp
  (testing "Output for A+1 should be the same as for M+1"
    (is (= (lookup-comp "A+1") (lookup-comp "M+1")))))
