(ns hack-clj.core-test
  (:require [clojure.test :refer :all]
            [hack-clj.core :refer :all]))

(deftest test-a-instruction?
  (testing "should return true iff string begins with '@'"
    (is (= true (a-instruction? "@123")))))
