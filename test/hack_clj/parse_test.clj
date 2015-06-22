(ns hack-clj.parse-test
  (:require [hack-clj.parse :refer :all]
            [clojure.test :refer :all]))

(deftest code?-test
  (is (code? "@fake // inline comment "))
  (is (code? " D=MD"))
  (is (not (code? "//I'm just a comment")))
  (is (not (code? " "))))

(deftest binary-strings-test
  (testing "round-trippable"
    (doseq [i (range -100 100)]
      (is (= i (-> i int->binstring binstring->int))))))

(deftest zfill-test
  (testing "can be called with arity 1"
    (let [res (zfill "x")]
      (is (= "00000000000000x" res))))
  (testing "can be called with arity 2"
    (let [res (zfill "x" 5)]
      (is (= "0000x")))))
