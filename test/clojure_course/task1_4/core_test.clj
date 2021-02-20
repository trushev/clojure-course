(ns clojure-course.task1-4.core-test
  (:require [clojure.test :refer :all]
            [clojure-course.task1-4.core :refer :all]
            [clojure-course.common.core-test :refer :all]))

(deftest test-words-from-alphabet-by-reduce
  (base-test-wfa wfa-by-reduce))
