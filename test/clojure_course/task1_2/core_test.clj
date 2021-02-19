(ns clojure-course.task1-2.core-test
  (:require [clojure.test :refer :all]
            [clojure-course.task1-2.core :refer :all]))

(deftest test
  (is (=
       `("cb" "ca" "bc" "ba" "ac" "ab")
       (words-from-alphabet-by-tail-recur `("a" "b" "c") 2)))
  (is (=
       `("aba" "bab")
       (words-from-alphabet-by-tail-recur `("a" "b") 3)))
  (is (=
       `()
       (words-from-alphabet-by-tail-recur `("a") 0))))
