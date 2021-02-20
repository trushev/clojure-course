(ns clojure-course.task1-1.wfa-test
  (:require [clojure.test :refer :all]
            [clojure-course.task1-1.wfa :refer :all]
            [clojure-course.common.util-test :refer :all]))

(deftest test-words-from-alphabet
  (base-test-wfa words-from-alphabet))
