(ns hack-clj.core-test
  (:require [clojure.test :refer :all]
            [hack-clj.core :refer :all]))

(deftest test-a-instruction?
  (testing "should return true iff string begins with '@'"
    (is (= true (a-instruction? "@123")))))

(deftest test-compile-a-instruction
  (testing "should always return a 16-digit binary number"
    (is (= (.length (compile-a-instruction "@123")) 16))))
