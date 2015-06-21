(ns hack-clj.vm-test
  (:require [hack-clj.vm :refer :all]
            [hack-clj.asm-interp :as interp]
            [clojure.test :refer :all]))

(defn run-ffi
  "Given a vector of hack assembly (including sugar) and an optional map of
  variables, call the given code and return the final value for the variable
  'out'. If there is no 'out', return all the vars."
  ([code] (run-ffi code {}))
  ([code vars]
   (let [results (interp/run code vars)]
     (get-in results [:vars "out"] results))))

(deftest run-ffi-test
  (testing "when 'out' is not in the symbol table"
    (let [no-out-asm ["@41"  "D=A+1" "@16" "M=D"
                      "@667" "D=A-1" "@17" "M=D"]]
      (testing "returns all the memory"
        (let [mem (run-ffi no-out-asm)]
          (is (= {16 42, 17 666}
                 (dissoc mem :vars :registers)))))))
  (testing "when 'out' is in the symbol table"
    (let [inc-asm ["@16" "D=M" "@out" "M=D+1"]]
      (testing "returns the value of 'out'"
        (is (= 43 (run-ffi inc-asm {16 42})))
        (is (= -11 (run-ffi inc-asm {16 -12})))))))

(defn run-stack
  [code]
  (let [final-mem (interp/run code)
        sp (get final-mem 0)
        stack-ptrs (range 256 sp)
        stack-map (select-keys final-mem stack-ptrs)]
    (->> stack-map
      sort
      (map second))))

(deftest constants
  (testing "Push constant"
    (testing "works for one number"
      (is (= '(33)
             (run-stack (wrap-init (push-constant 33)))))
      (is (= '(42)
             (run-stack (wrap-init (push-constant 42))))))
    (testing "works for multiple numbers"
      (let [nums-asm (wrap-init (mapv push-constant [4 8 15 16 23 42]))]
        (is (= '(4 8 15 16 23 42)
               (run-stack nums-asm)))))))

(deftest arithmetic
  (testing "add"
    (testing "two numbers"
      (testing "empty stack otherwise"
        (let [add-3-5 (wrap-init [(push-constant 3)
                                  (push-constant 5)
                                  add])]
          (is (= '(8) (run-stack add-3-5)))))
      (testing "with other numbers on the stack"
        (let [add-3-5-ignore-12 (wrap-init [(push-constant 12)
                                            (push-constant 3)
                                            (push-constant 5)
                                            add])]
          (is (= '(12 8) (run-stack add-3-5-ignore-12))))))

    (testing "with multiple numbers"
      (let [add-1-2-3-4 (wrap-init [(push-constant 666)
                                    (push-constant 1)
                                    (push-constant 2)
                                    (push-constant 3)
                                    (push-constant 4)
                                    add
                                    add
                                    add])]
        (is (= '(666 10) (run-stack add-1-2-3-4))))))

  (testing "sub"
    (testing "two numbers"
      (let [sub-8-5 (wrap-init [(push-constant 8)
                                (push-constant 5) ;; yes, this is the correct order
                                sub])]
        (is (= '(3) (run-stack sub-8-5)))))
    (testing "with multiple numbers"
      (let [sub-100-68-10 (wrap-init [(push-constant 720)
                                      (push-constant 100)
                                      (push-constant 68)
                                      (push-constant 10)
                                      sub
                                      sub])]
        (is (= '(720 42) (run-stack sub-100-68-10))))))

  (testing "neg"
    (testing "with one number on the stack"
      (let [neg-100 (wrap-init [(push-constant 100)
                                neg])]
        (is (= '(-100) (run-stack neg-100)))))
    (testing "with two numbers on the stack"
      (let [neg-50 (wrap-init [(push-constant 301)
                               (push-constant 50)
                               neg])]
        (is (= '(301 -50) (run-stack neg-50)))))
    (testing "is surjective"
      (let [neg-neg-12 (wrap-init [(push-constant 12)
                                   neg
                                   neg])]
        (is (= '(12) (run-stack neg-neg-12))))))

  (testing "eq"
    (let [eq-ffi (fn [x y] (-> [(push-constant x) (push-constant y) (eq)]
                             wrap-init
                             run-stack
                             first))]
      (are [pair expected] (= expected (apply eq-ffi pair))
        [1 1]       TRUE
        [0 0]       TRUE
        [100 100]   TRUE
        [-12 -12]   TRUE
        [1234 1234] TRUE

        [1 0]       FALSE
        [0 1]       FALSE
        [1000 100]  FALSE
        [100 1000]  FALSE
        [100 101]   FALSE
        [1234 1235] FALSE
        [67 200]    FALSE))))
