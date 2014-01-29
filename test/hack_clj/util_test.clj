(ns hack-clj.util-test
  (:require [clojure.test :refer :all]
            [hack-clj.util :refer :all]))

(deftest test-pad
  (testing "should add thirteen zeroes to 111"
    (is (= 16 (.length (pad "111" 16))))))
