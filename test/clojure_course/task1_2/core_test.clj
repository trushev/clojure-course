(ns clojure-course.task1-2.core-test
  (:require [clojure.test :refer :all]
            [clojure-course.task1-2.core :refer :all]
            [clojure-course.common.core-test :refer :all]))

(deftest test-wfa-by-tail-recur
  (base-test-wfa wfa-by-tail-recur))
