(ns hack-clj.vm-test
  (:require [clojure.string :as str]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test :refer :all]
            [hack-clj.asm-interp :as interp]
            [hack-clj.test-utils :as tu]
            [hack-clj.parse :as parse]
            [hack-clj.vm :refer :all]
            [hack-clj.vm.analyzer :as analyzer])
  (:refer-clojure :exclude [pop]))

(defn run-mem
  "Given a vector of hack assembly (including sugar) and an optional map of
  variables, call the given code and return the final value for the variable
  'out'. If there is no 'out', return all the vars."
  ([code] (run-mem code {}))
  ([code vars]
   (let [results (interp/run code vars)]
     (get-in results [:vars "out"] results))))

(defn is-empty-stack?
  [mem]
  (testing "leaves the stack empty"
    (is (= 256 (get mem 0)))))

(deftest run-mem-test
  (testing "when 'out' is not in the symbol table"
    (let [no-out-asm ["@41"  "D=A+1" "@16" "M=D"
                      "@667" "D=A-1" "@17" "M=D"]]
      (testing "returns all the memory"
        (let [mem (run-mem no-out-asm)]
          (is (= {16 42, 17 666}
                 (dissoc mem :vars :registers)))))))

  (testing "when 'out' is in the symbol table"
    (let [inc-asm ["@16" "D=M" "@out" "M=D+1"]]
      (testing "returns the value of 'out'"
        (is (= 43 (run-mem inc-asm {16 42})))
        (is (= -11 (run-mem inc-asm {16 -12})))))))

(defn run-stack
  [code]
  (let [final-mem (interp/run code)
        sp (get final-mem 0)
        stack-ptrs (range 256 sp)
        stack-map (select-keys final-mem stack-ptrs)]
    (->> stack-map
      sort
      (map second))))

(defn wrap-vars
  [code vars]
  (flattenv
   [(for [[var val] vars]
      [(d-load val)
       (a-load var)
       "M=D"])
    code]))

(def gen-short
  (gen/choose java.lang.Short/MIN_VALUE
              java.lang.Short/MAX_VALUE))

(defn passes?
  [quick-check]
  (let [result (:result quick-check)]
    (if (true? result)
      (is true)
      (is (= true quick-check)))))

(deftest arithmetic-test
  (testing "add"
    (testing "two numbers"
      (testing "empty stack otherwise"
        (let [add-3-5 (wrap-init [(push-constant 3)
                                  (push-constant 5)
                                  (arithmetic "add")])]
          (is (= '(8) (run-stack add-3-5)))))

      (testing "with other numbers on the stack"
        (let [add-3-5-ignore-12 (wrap-init [(push-constant 12)
                                            (push-constant 3)
                                            (push-constant 5)
                                            (arithmetic "add")])]
          (is (= '(12 8) (run-stack add-3-5-ignore-12))))))

    (testing "with multiple numbers"
      (let [add-1-2-3-4 (wrap-init [(push-constant 666)
                                    (push-constant 1)
                                    (push-constant 2)
                                    (push-constant 3)
                                    (push-constant 4)
                                    (arithmetic "add")
                                    (arithmetic "add")
                                    (arithmetic "add")])]
        (is (= '(666 10) (run-stack add-1-2-3-4)))))

    (testing "generative tests"
      (let [add-ffi (fn [x y] (-> [(push-constant x)
                                   (push-constant y)
                                   (arithmetic "add")]
                                wrap-init
                                run-stack
                                first))
            add-prop (prop/for-all [x gen-short
                                    y gen-short]
                       (= (+ x y)
                          (add-ffi x y)))]
        (passes? (tc/quick-check 100 add-prop)))))

  (testing "sub"
    (testing "two numbers"
      (let [sub-8-5 (wrap-init [(push-constant 8)
                                (push-constant 5) ;; yes, this is the correct order
                                (arithmetic "sub")])]
        (is (= '(3) (run-stack sub-8-5))))

      (testing "generative"
        (let [sub-ffi (fn [x y] (-> [(push-constant x)
                                     (push-constant y)
                                     (arithmetic "sub")]
                                  wrap-init
                                  run-stack
                                  first))
              sub-prop (prop/for-all [x gen-short
                                      y gen-short]
                         (= (- x y)
                            (sub-ffi x y)))]
          (passes? (tc/quick-check 100 sub-prop)))))

    (testing "with multiple numbers"
      (let [sub-100-68-10 (wrap-init [(push-constant 720)
                                      (push-constant 100)
                                      (push-constant 68)
                                      (push-constant 10)
                                      (arithmetic "sub")
                                      (arithmetic "sub")])]
        (is (= '(720 42) (run-stack sub-100-68-10))))))

  (testing "neg"
    (let [neg-ffi (fn [x] (-> [(push-constant x)
                               (arithmetic "neg")]
                            wrap-init
                            run-stack
                            first))]
      (testing "negates numbers"
        (passes? (tc/quick-check 100
                   (prop/for-all [x gen-short]
                     (= (- x)
                        (neg-ffi x))))))

      (testing "is surjective"
        (passes? (tc/quick-check 100
                   (prop/for-all [x gen-short]
                     (= x
                        (neg-ffi (neg-ffi x))))))))

    (testing "with two numbers on the stack"
      (let [neg-50 (wrap-init [(push-constant 301)
                               (push-constant 50)
                               (arithmetic "neg")])]
        (is (= '(301 -50) (run-stack neg-50))))))

  (testing "eq"
    (let [eq-ffi (fn [x y] (-> [(push-constant x) (push-constant y) (arithmetic "eq")]
                             wrap-init
                             run-stack
                             first))]
      (testing "identity"
        (passes? (tc/quick-check 100
                   (prop/for-all [x gen-short]
                     (= TRUE (eq-ffi x x))))))

      (testing "false cases"
        (passes? (tc/quick-check 100
                   (prop/for-all [x gen-short
                                  y gen-short]
                     (if (= x y)
                       (= TRUE (eq-ffi x y))
                       (= FALSE (eq-ffi x y)))))))))

  (testing "gt"
    (let [gt-ffi (fn [x y] (-> [(push-constant x) (push-constant y) (arithmetic "gt")]
                             wrap-init
                             run-stack
                             first))]
      (passes? (tc/quick-check 200
                 (prop/for-all [x gen-short
                                y gen-short]
                   (if (> x y)
                     (= TRUE (gt-ffi x y))
                     (= FALSE (gt-ffi x y))))))))

  (testing "lt"
    (let [lt-ffi (fn [x y] (-> [(push-constant x) (push-constant y) (arithmetic "lt")]
                             wrap-init
                             run-stack
                             first))]
      (passes? (tc/quick-check 200
                 (prop/for-all [x gen-short
                                y gen-short]
                   (if (< x y)
                     (= TRUE (lt-ffi x y))
                     (= FALSE (lt-ffi x y)))))))))

(deftest boolean-operations
  (testing "b-and"
    (let [and-ffi (fn [x y] (-> [(push-constant x) (push-constant y) (arithmetic "and")]
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
    (let [or-ffi (fn [x y] (-> [(push-constant x) (push-constant y) (arithmetic "or")]
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
    (let [not-ffi (fn [x] (-> [(push-constant x) (arithmetic "not")]
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
                  run-mem)
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
                    run-mem)]
          (is-empty-stack? mem)

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
                    run-mem)]
          (is-empty-stack? mem)

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
                  run-mem)]
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
                    run-mem)
              local-segment (->> (select-keys mem (range 100 103))
                              sort
                              (mapv second))]
          (is-empty-stack? mem)

          (testing "leaves the correct values in the segment"
            (is (= [23 29 31] local-segment))))))))

(deftest push-test
  (testing "pushing"
    (testing "constants"
      (testing "works for one number"
        (passes? (tc/quick-check 100
                   (prop/for-all [x gen-short]
                     (= [x]
                        (run-stack (wrap-init (push {:segment "constant", :offset x}))))))))

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
                    (wrap-vars {"Anonymous.2" 42})
                    wrap-init
                    run-stack)]
        (is (= [32 42 52] stack))))

    (testing "pointers:"
      (testing "this"
        (let [stack (-> [(set-mem 3 61) ;; init this pointer
                         (push-constant 310)
                         (push {:segment "pointer", :offset 0})
                         (push-constant 11)]
                      wrap-init
                      run-stack)]
          (is (= [310 61 11] stack))))

      (testing "that"
        (let [stack (-> [(set-mem 4 217) ;; init that pointer
                         (push-constant 333)
                         (push {:segment "pointer", :offset 1})
                         (push-constant 213)]
                      wrap-init
                      run-stack)]
          (is (= [333 217 213] stack)))))

    (testing "from the dynamic segments:"
      (testing "local"
        (let [stack (-> [(set-mem 1 777) ;; init local pointer
                         (set-mem 779 1234) ;; set offset 2 = 1234
                         (push {:segment "local", :offset 2})]
                      wrap-init
                      run-stack)]

          (is (= [1234] stack))))

      (testing "argument"
        (let [stack (-> [(set-mem 2 900) ;; init argument pointer
                         (set-mem 900 71) ;;set offset 0 = 71
                         (push-constant 70)
                         (push {:segment "argument", :offset 0})
                         (set-mem 901 73) ;; set offset 1 = 73
                         (push {:segment "argument", :offset 1})
                         (set-mem 902 75) ;; set offset 2 = 75
                         (push {:segment "argument", :offset 2})
                         (push-constant 76)]
                      wrap-init
                      run-stack)]
          (is (= [70 71 73 75 76] stack)))))))

(deftest program-flow-test
  (testing "label"
    (testing "emits an appropriate label"
      (testing "when given a namespace"
        (let [program (-> [(push-constant 220)
                           (label {:ns "Test" :name "LOOP_START"})
                           (push-constant 300)]
                          wrap-init)]
          (is (some #{"(Test.LOOP_START)"} program))
          (is (= [220 300] (run-stack program)))))
      (testing "when the namespace is omitted"
        (let [program (-> [(push-constant 333)
                           (label {:name "FruitLoops"})
                           (push-constant 13)]
                          wrap-init)]
          (is (some #{"(Anonymous.FruitLoops)"} program))
          (is (= [333 13] (run-stack program)))))))

  (testing "goto"
    (let [stack (-> [(push-constant 666)
                     (goto {:ns "Test", :target "end"})
                     (push-constant 420) ;; should be skipped
                     (label {:ns "Test", :name "end"})]
                    wrap-init
                    run-stack)]
      (is (= [666] stack))))

  (testing "if-goto"
    (testing "without a jump"
      (let [stack (-> [(push-constant 123)
                       (push-constant FALSE)
                       (if-goto {:ns "Test", :target "end"})
                       (push-constant 456)
                       (label {:ns "Test", :name "end"})]
                      wrap-init
                      run-stack)]
        (is (= [123 456] stack))))

    (testing "with a jump"
      (let [stack (-> [(push-constant 123)
                       (push-constant TRUE)
                       (if-goto {:ns "Test", :target "PushSixes"})
                       (goto {:ns "Test", :target "end"})
                       (label {:ns "Test", :name "PushSixes"})
                       (push-constant 666)
                       (label {:ns "Test", :name "end"})]
                      wrap-init
                      run-stack)]
        (is (= [123 666] stack))))))

(defn run-code-with-book-acceptance
  "Takes a vector of path elements relative to dev-resources/vm"
  [path-vec code]
  (let [path (str/join "/" path-vec)
        dir (str "dev-resources/vm/" path)
        test-name (last path-vec)
        base (str dir "/" test-name)
        init-mem (tu/read-tst (str base ".tst"))
        cmp-mem (tu/read-cmp (str base ".cmp"))
        mem (run-mem code init-mem)]
    (doseq [[loc val] cmp-mem]
      (is (= val (get cmp-mem loc))))))

(defn run-book-acceptance
  "Takes a vector of path elements relative to dev-resources/vm"
  [path-vec]
  (let [path (str/join "/" path-vec)
        dir (str "dev-resources/vm/" path)
        test-name (last path-vec)
        base (str dir "/" test-name)
        in-vm (str base ".vm")
        lines (-> in-vm parse/file->lines parse/strip-comments)
        name (last path-vec)
        code (emit-asm name (analyzer/analyze lines))
        init-mem (tu/read-tst (str base ".tst"))
        cmp-mem (tu/read-cmp (str base ".cmp"))
        mem (run-mem code init-mem)]
    (doseq [[loc val] cmp-mem]
      (is (= val (get cmp-mem loc))))))

(deftest acceptance-test
  (testing "BasicTest (manually parsed)"
    (let [code (flattenv
                [(push {:segment "constant", :offset 10})
                 (pop {:segment "local", :offset 0})
                 (push {:segment "constant", :offset 21})
                 (push {:segment "constant", :offset 22})
                 (pop {:segment "argument", :offset 2})
                 (pop {:segment "argument", :offset 1})
                 (push {:segment "constant", :offset 36})
                 (pop {:segment "this", :offset 6})
                 (push {:segment "constant", :offset 42})
                 (push {:segment "constant", :offset 45})
                 (pop {:segment "that", :offset 5})
                 (pop {:segment "that", :offset 2})
                 (push {:segment "constant", :offset 510})
                 (pop {:segment "temp", :offset 6})
                 (push {:segment "local", :offset 0})
                 (push {:segment "that", :offset 5})
                 (arithmetic "add")
                 (push {:segment "argument", :offset 1})
                 (arithmetic "sub")
                 (push {:segment "this", :offset 6})
                 (push {:segment "this", :offset 6})
                 (arithmetic "add")
                 (arithmetic "sub")
                 (push {:segment "temp", :offset 6})
                 (arithmetic "add")])]
      (run-code-with-book-acceptance ["MemoryAccess" "BasicTest"] code)))

  (testing "MemoryAccess"
    (testing "BasicTest"
      (run-book-acceptance ["MemoryAccess" "BasicTest"]))

    (testing "PointerTest"
      (run-book-acceptance ["MemoryAccess" "PointerTest"])))

  (testing "StackArithmetic"
    (testing "SimpleAdd"
      (run-book-acceptance ["StackArithmetic" "SimpleAdd"]))

    (testing "StackTest"
      (run-book-acceptance ["StackArithmetic" "StackTest"])))

  (testing "ProgramFlow"
    (testing "BasicLoop"
      (run-book-acceptance ["ProgramFlow" "BasicLoop"]))))
