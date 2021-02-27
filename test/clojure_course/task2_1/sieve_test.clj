(ns clojure-course.task2-1.sieve-test
  (:require [clojure.test :refer :all]
            [clojure-course.task2-1.sieve :refer :all]))

(deftest test-nth-prime
  (is (= 2 (prime 1)))
  (is (= 3 (prime 2)))
  (is (= 7 (prime 4)))
  (is (= 29 (prime 10)))
  (is (= 127 (prime 31)))
  (is (= 263 (prime 56))))
