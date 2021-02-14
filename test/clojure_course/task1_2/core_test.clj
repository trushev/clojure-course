(ns clojure-course.task1-2.core-test
  (:require [clojure.test :refer :all]
            [clojure-course.task1-2.core :refer :all]))

(deftest test1
  (is (=
       `("cb" "ca" "bc" "ba" "ac" "ab")
       (task1-2 `("a" "b" "c") 2))))

(deftest test2
  (is (=
       `("aba" "bab")
       (task1-2 `("a" "b") 3))))

(deftest test3
  (is (=
       `()
       (task1-2 `("a") 0))))
