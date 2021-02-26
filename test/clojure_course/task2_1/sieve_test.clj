(ns clojure-course.task2-1.sieve-test
  (:require [clojure.test :refer :all]
            [clojure-course.task2-1.sieve :refer :all]))

(deftest test-prime
  (is (= 2 (prime 2)))
  (is (= 7 (prime 4)))
  (is (= 29 (prime 10))))
