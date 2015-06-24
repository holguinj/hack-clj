(ns hack-clj.vm-test
  (:require [hack-clj.vm :refer :all]
            [hack-clj.asm-interp :as interp]
            [clojure.test :refer :all])
  (:refer-clojure :exclude [pop]))

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
  ([code] (run-stack code {}))
  ([code init-vars]
   (let [final-mem (interp/run code init-vars)
         sp (get final-mem 0)
         stack-ptrs (range 256 sp)
         stack-map (select-keys final-mem stack-ptrs)]
     (->> stack-map
       sort
       (map second)))))

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
        (is (= '(3) (run-stack sub-8-5))))
      (let [sub-ffi (fn [x y] (-> [(push-constant x)
                                   (push-constant y)
                                   sub]
                                wrap-init
                                run-stack
                                first))]
        (are [pair expected] (= expected (apply sub-ffi pair))
          [100 99] 1
          [1 1]    0
          [33 10]  23
          [0 10]   -10
          [10 30]  -20)))

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
        [67 200]    FALSE)))

  (testing "gt"
    (let [gt-ffi (fn [x y] (-> [(push-constant x) (push-constant y) (gt)]
                             wrap-init
                             run-stack
                             first))]
      (are [pair expected] (= expected (apply gt-ffi pair))
        [1 0]        TRUE
        [1 -1]       TRUE
        [100 99]     TRUE
        [0 -1]       TRUE
        [-100 -1000] TRUE

        [0 0]       FALSE
        [0 1]       FALSE
        [-100 0]    FALSE
        [1000 1001] FALSE
        [1111 1111] FALSE)))

  (testing "lt"
    (let [lt-ffi (fn [x y] (-> [(push-constant x) (push-constant y) (lt)]
                             wrap-init
                             run-stack
                             first))]
      (are [pair expected] (= expected (apply lt-ffi pair))
        [1 0]        FALSE
        [1 -1]       FALSE
        [100 99]     FALSE
        [0 -1]       FALSE
        [-100 -1000] FALSE
        [0 0]        FALSE
        [1111 1111]  FALSE

        [0 1]       TRUE
        [-100 0]    TRUE
        [1000 1001] TRUE
        [-100 -99]  TRUE))))

(deftest boolean-operations
  (testing "b-and"
    (let [and-ffi (fn [x y] (-> [(push-constant x) (push-constant y) b-and]
                              wrap-init
                              run-stack
                              first))]
      (are [pair expected] (= expected (apply and-ffi pair))
        [12 13]   12
        [13 12]   12
        [0 1]     0
        [1 0]     0
        [0 255]   0
        [255 0]   0
        [666 666] 666
        [100 50]  32
        [50 100]  32)))

  (testing "b-or"
    (let [or-ffi (fn [x y] (-> [(push-constant x) (push-constant y) b-or]
                              wrap-init
                              run-stack
                              first))]
      (are [pair expected] (= expected (apply or-ffi pair))
        [0 1]   1
        [1 0]   1
        [1 12]  13
        [12 1]  13
        [13 25] 29
        [25 13] 29)))

  (testing "b-not"
    (let [not-ffi (fn [x] (-> [(push-constant x) b-not]
                              wrap-init
                              run-stack
                              first))]
      (are [x expected] (= expected (not-ffi x))
        -13   12
        12    -13
        -1    0
        0     -1))))

(defn set-mem
  [loc val]
  (flattenv
   [(d-load val)
    (a-load loc)
    "M=D"]))

(deftest pop-test
  (testing "popping"
    (testing "numbers into the temp segment"
      (let [mem (-> [(push-constant 1)
                     (pop {:segment "temp", :offset 0})
                     (push-constant 1)
                     (pop {:segment "temp", :offset 1})
                     (push-constant 2)
                     (pop {:segment "temp", :offset 2})
                     (push-constant 3)
                     (pop {:segment "temp", :offset 3})
                     (push-constant 5)
                     (pop {:segment "temp", :offset 4})
                     (push-constant 8)
                     (pop {:segment "temp", :offset 5})
                     (push-constant 13)
                     (pop {:segment "temp", :offset 6})]
                  wrap-init
                  run-ffi)
            temp-segment (->> (select-keys mem (range 5 13))
                           sort
                           (mapv second))]
        (testing "results in an empty stack"
          (is (= 256 (get mem 0))))

        (testing "leaves them all in the expected place"
          (is (= [1 1 2 3 5 8 13]
                 temp-segment)))))

    (testing "pointers:"
      (testing "with offset 0"
        (let [mem (-> [(push-constant 666)
                       (pop {:segment "pointer", :offset 0})
                       (push-constant 333)
                       (pop {:segment "this", :offset 0})
                       (push-constant 999)
                       (pop {:segment "this", :offset 1})]
                    wrap-init
                    run-ffi)]
          (testing "leaves the stack empty"
            (is (= 256 (get mem 0))))

          (testing "correctly initializes the 'this' segment"
            (is (= 666 (get mem 3)))
            (let [base 666]
              (is (= 333
                     (get mem (+ base 0))))
              (is (= 999
                     (get mem (+ base 1))))))))

      (testing "with offset 1"
        (let [mem (-> [(push-constant 300)
                       (pop {:segment "pointer", :offset 1})
                       (push-constant 27)
                       (pop {:segment "that", :offset 0})
                       (push-constant 29)
                       (pop {:segment "that", :offset 1})]
                    wrap-init
                    run-ffi)]
          (testing "leaves the stack empty"
            (is (= 256 (get mem 0))))

          (testing "correctly initializes the 'this' segment"
            (is (= 300 (get mem 4)))
            (let [base 300]
              (is (= 27
                     (get mem (+ base 0))))
              (is (= 29
                     (get mem (+ base 1)))))))))

    (testing "to the static segment"
      (let [mem (-> [(push-constant 10)
                     (pop {:segment "static", :offset 0})
                     (push-constant 20)
                     (pop {:segment "static", :offset 1})
                     (push-constant 30)
                     (pop {:segment "static", :offset 2})
                     (push-constant 40)
                     (pop {:segment "static", :offset 3})]
                  wrap-init
                  run-ffi)]
        (testing "results in an empty stack"
          (is (= 256 (get mem 0))))

        (testing "creates the appropriate variables"
          (are [var val] (= val (get-in mem [:vars var]))
            "Anonymous.0" 10
            "Anonymous.1" 20
            "Anonymous.2" 30
            "Anonymous.3" 40))))

    (testing "with dynamic segments:"
      (testing "pushing to the local segment"
        (let [mem (-> [(set-mem 1 100)
                       (push-constant 23)
                       (pop {:segment "local", :offset 0})
                       (push-constant 29)
                       (pop {:segment "local", :offset 1})
                       (push-constant 31)
                       (pop {:segment "local", :offset 2})]
                    wrap-init
                    run-ffi)
              local-segment (->> (select-keys mem (range 100 103))
                              sort
                              (mapv second))]
          (testing "leaves the stack empty"
            (is (= 256 (get mem 0))))

          (testing "leaves the correct values in the segment"
            (is (= [23 29 31] local-segment))))))))

(deftest push-test
  (testing "pushing"
    (testing "constants"
      (testing "works for one number"
        (is (= '(33)
               (run-stack (wrap-init (push {:segment "constant", :offset 33})))))
        (is (= '(42)
               (run-stack (wrap-init (push {:segment "constant", :offset 42}))))))

      (testing "works for multiple numbers"
        (let [nums-asm (wrap-init (mapv #(push {:segment "constant", :offset %})
                                        [4 8 15 16 23 42]))]
          (is (= '(4 8 15 16 23 42)
                 (run-stack nums-asm)))))

      (testing "works for negative numbers"
        (let [negs-asm (wrap-init (mapv #(push {:segment "constant", :offset %})
                                        [-1 -10 -100 -1000]))]
          (is (= '(-1 -10 -100 -1000)
                 (run-stack negs-asm))))))

    (testing "from the temp segment"
      (let [stack (-> [(set-mem 5 11)
                       (push {:segment "temp", :offset 0})
                       (set-mem 6 12)
                       (push {:segment "temp", :offset 1})
                       (set-mem 7 13)
                       (push {:segment "temp", :offset 2})
                       (set-mem 8 14)
                       (push {:segment "temp", :offset 3})
                       (set-mem 9 15)
                       (push {:segment "temp", :offset 4})]
                    wrap-init
                    run-stack)]
        (testing "places the values on the stack"
          (is (= '(11 12 13 14 15) stack)))))

    (testing "from the 'static' segment"
      (let [stack (-> [(push-constant 32)
                       (push {:segment "static", :offset 2})
                       (push-constant 52)]
                    wrap-init
                    (run-stack {"Anonymous.2" 42}))]
        (= [32 42 52] stack)))

    (testing "pointers:"
      (testing "this"
        (let [stack (-> [(set-mem 3 61)
                         (push-constant 310)
                         (push {:segment "pointer", :offset 0})
                         (push-constant 11)]
                      wrap-init
                      run-stack)]
          (= [310 61 11] stack)))

      (testing "that"
        (let [stack (-> [(set-mem 4 217)
                         (push-constant 333)
                         (push {:segment "pointer", :offset 1})
                         (push-constant 213)]
                      wrap-init
                      run-stack)]
          (= [333 217 213] stack))))))
