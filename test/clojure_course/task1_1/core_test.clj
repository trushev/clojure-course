(ns clojure-course.task1-1.core-test
  (:require [clojure.test :refer :all]
            [clojure-course.task1-1.core :refer :all]
            [clojure-course.common.core-test :refer :all]))

(deftest test-words-from-alphabet
  (base-test-wfa words-from-alphabet))
