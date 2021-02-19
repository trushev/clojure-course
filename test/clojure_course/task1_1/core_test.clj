(ns clojure-course.task1-1.core-test
  (:require [clojure.test :refer :all]
            [clojure-course.task1-1.core :refer :all]))

(deftest test
  (is (=
       `("cb" "ca" "bc" "ba" "ac" "ab")
       (words-from-alphabet `("a" "b" "c") 2)))
  (is (=
       `("aba" "bab")
       (words-from-alphabet `("a" "b") 3)))
  (is (=
       `()
       (words-from-alphabet `("a") 0))))
