(ns clojure-course.task1-4.reduce-wfa-test
  (:require [clojure.test :refer :all]
            [clojure-course.task1-4.reduce-wfa :refer :all]
            [clojure-course.common.util-test :refer :all]))

(deftest test-words-from-alphabet-by-reduce
  (base-test-wfa wfa-by-reduce))
