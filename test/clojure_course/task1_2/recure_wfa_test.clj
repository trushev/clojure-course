(ns clojure-course.task1-2.recure-wfa-test
  (:require [clojure.test :refer :all]
            [clojure-course.task1-2.recure-wfa :refer :all]
            [clojure-course.common.util-test :refer :all]))

(deftest test-wfa-by-tail-recur
  (base-test-wfa wfa-by-tail-recur))
