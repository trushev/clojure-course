(ns clojure-course.task2-2.lazy-primes-test
  (:require [clojure.test :refer :all]
            [clojure-course.task2-2.lazy-primes :refer :all]))

(deftest test-nth-prime
  (is (= 2 (nth lazy-primes 0)))
  (is (= 3 (nth lazy-primes 1)))
  (is (= 7 (nth lazy-primes 3)))
  (is (= 29 (nth lazy-primes 9)))
  (is (= 127 (nth lazy-primes 30)))
  (is (= 263 (nth lazy-primes 55))))
