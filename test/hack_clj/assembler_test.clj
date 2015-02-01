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

(deftest compile-c-instruction-test
  (testing "book examples"
    (is (= "1111110111011000"
           (compile-c-instruction {:dest "MD"
                                   :comp "M+1"})))
    (is (= "1110010011010000"
         (compile-c-instruction {:dest "D"
                                 :comp "D-A"})))))
