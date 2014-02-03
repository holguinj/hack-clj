(ns hack-clj.vm-test
  (:require [clojure.test :refer :all]
            [hack-clj.vm :refer :all]))

(deftest test-command-type
  (testing "should output :c-arithmetic for add/sub/neg/etc"
    (is (= :c-arithmetic (command-type "neg")))
    (is (= :c-arithmetic (command-type "and")))
    (is (= :c-arithmetic (command-type "sub")))
    (is (= :c-arithmetic (command-type "add"))))
  (testing "should output :c-push for push"
    (is (= :c-push (command-type "push"))))
  (testing "should output :c-pop for pop"
    (is (= :c-pop (command-type "pop"))))
  (testing "should output :c-label for label"
    (is (= :c-label (command-type "label"))))
  (testing "should output :c-function"
    (is (= :c-function (command-type "function"))))
  (testing "should output :c-return for return"
    (is (= :c-return (command-type "return"))))
  (testing "should output :c-call for call"
    (is (= :c-call (command-type "call")))))
