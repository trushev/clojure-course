(ns clojure-course.task3-2.lazy-integral-test
  (:require [clojure.test :refer :all]
            [clojure-course.task3-2.lazy-integral :refer :all]))

(defn foo-1 [_] 1)
(defn foo-x [x] x)
(defn bar [_] (Thread/sleep 0.5) 0)

(deftest test-f-square
  (is (= 0.5 (f-square foo-x 0 1)))
  (is (= 1.5 (f-square foo-x 1 1))))

(deftest test-operator-time
  (let [prim (operator bar 0.2)]
    (time (prim 20))
    (time (prim 20.33))
    (time (prim 40))
    (time (prim 40.09))
    (time (prim 15))))

(deftest test-operator-value
  (let [prim-1 (operator foo-1 1)
        prim-x (operator foo-x 0.1)]
    (is (= 1.99 (prim-1 1.99)))
    (is (= 2.0 (prim-1 2.0)))
    (is (= 2.01 (prim-1 2.01)))
    (is (= 2.0 (prim-x 2.0)))
    (is (= 12.5 (prim-x 5.0)))))
