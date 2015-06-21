(ns hack-clj.parse-test
  (:require [hack-clj.parse :refer :all]
            [clojure.test :refer :all]))

(deftest code?-test
  (is (code? "@fake // inline comment "))
  (is (code? " D=MD"))
  (is (not (code? "//I'm just a comment")))
  (is (not (code? " "))))
