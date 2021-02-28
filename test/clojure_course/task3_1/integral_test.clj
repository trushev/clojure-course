(ns clojure-course.task3-1.integral-test
  (:require [clojure.test :refer :all]
            [clojure-course.task3-1.integral :refer :all]))

(defn foo-1 [x] 1)
(defn foo-x [x] x)

(deftest test-f-square
  (is (= 0.5 (f-square foo-x 0 1)))
  (is (= 1.5 (f-square foo-x 1 1))))

(deftest test-operator-time
  (let [prim-1 (operator foo-1 0.2)]
    ((operator foo-1 0.25) 20) ; jvm warm up
    (time (prim-1 20))
    (time (prim-1 20))
    (time (prim-1 40))
    (time (prim-1 40))
    (time (prim-1 15))))

(deftest test-operator-value
  (let [prim-1 (operator foo-1 1)
        prim-x (operator foo-x 0.1)]
    (is (= 1.99 (prim-1 1.99)))
    (is (= 2.0 (prim-1 2.0)))
    (is (= 2.01 (prim-1 2.01)))
    (is (= 2.0 (prim-x 2.0)))
    (is (= 12.5 (prim-x 5.0)))))
