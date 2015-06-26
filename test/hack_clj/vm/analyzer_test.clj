(ns hack-clj.vm.analyzer-test
  (:require [hack-clj.vm.analyzer :refer :all]
            [clojure.test :refer :all]))

(deftest arithmetic-operations
  (testing "add, sub, neg"
    (is (= [:arithmetic "add"]
           (analyze-line "add")))
    (is (= [:arithmetic "sub"]
           (analyze-line "sub")))
    (is (= [:arithmetic "neg"]
           (analyze-line "neg"))))

  (testing "not, eq, lt, gt"
    (is (= [:arithmetic "not"]
           (analyze-line "not")))
    (is (= [:arithmetic "eq"]
           (analyze-line "eq")))
    (is (= [:arithmetic "lt"]
           (analyze-line "lt")))
    (is (= [:arithmetic "gt"]
           (analyze-line "gt")))))

(deftest memory-access
  (testing "push"
    (is (= [:push {:segment "constant", :offset 21}]
           (analyze-line "push constant 21")))
    (is (= [:push {:segment "local", :offset 999}]
           (analyze-line "push local 999"))))
  (testing "pop"
    (is (= [:pop {:segment "this", :offset 0}]
           (analyze-line "pop this 0")))
    (is (= [:pop {:segment "static", :offset 3}]
           (analyze-line "pop static 3")))))

