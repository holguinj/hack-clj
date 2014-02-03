(ns hack-clj.vm-test
  (:require [clojure.test :refer :all]
            [hack-clj.vm :refer :all]))

(comment (deftest test-pad
  (testing "should add thirteen zeroes to 111"
    (is (= 16 (.length (pad "111" 16)))))))
