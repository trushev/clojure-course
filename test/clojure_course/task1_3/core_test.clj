(ns clojure-course.task1-3.core-test
  (:require [clojure.test :refer :all]
            [clojure-course.task1-3.core :refer :all]))

(deftest test-my-map
  (is
   (=
    (range 1 5)
    (my-map inc (range 0 4))))
  (is
   (=
    `()
    (my-map inc `()))))

(deftest test-my-filter
  (is
   (=
    `(0 0 0)
    (my-filter (fn [x] x) `(0 nil 0 0 nil))))
  (is
   (=
    `()
    (my-filter (fn [x] x) `()))))
