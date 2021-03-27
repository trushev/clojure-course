(ns clojure-course.task4-1.cnf-test
  (:require [clojure.test :refer :all]
            [clojure-course.task4-1.cnf :refer :all]))

(def incorrect 0)

(deftest test-constant
  (is (constant? (constant true)))
  (is (constant? (constant false)))
  (is (= true (constant-value (constant true))))
  (is (= false (constant-value (constant false))))
  (is (thrown? AssertionError (constant incorrect)))
  (is (thrown? AssertionError (constant? incorrect)))
  (is (thrown? AssertionError (constant-value incorrect))))

(deftest test-variable
  (is (variable? (variable :x)))
  (is (= :x (variable-name (variable :x))))
  (is (same-variable? (variable :x) (variable :x)))
  (is (not (same-variable? (variable :x) (variable :y))))
  (is (thrown? AssertionError (variable incorrect)))
  (is (thrown? AssertionError (variable? incorrect)))
  (is (thrown? AssertionError (variable-name incorrect)))
  (is (thrown? AssertionError (same-variable? (variable :x) incorrect)))
  (is (thrown? AssertionError (same-variable? incorrect (variable :x))))
  (is (thrown? AssertionError (same-variable? incorrect incorrect))))

(deftest test-and
  (is (And? (And (variable :x) (variable :y))))
  (is (thrown? AssertionError (And incorrect)))
  (is (thrown? AssertionError (And? incorrect))))

(deftest test-or
  (is (Or? (Or (variable :x) (variable :y))))
  (is (thrown? AssertionError (Or incorrect)))
  (is (thrown? AssertionError (Or? incorrect))))

(deftest test-not
  (is (Not? (Not (variable :x))))
  (is (thrown? AssertionError (Not incorrect)))
  (is (thrown? AssertionError (Not? incorrect))))

(deftest test-impl
  (is (Impl? (Impl (variable :x) (variable :y))))
  (is (thrown? AssertionError (Impl incorrect incorrect)))
  (is (thrown? AssertionError (Impl? incorrect))))

(deftest test-expression
  (is (not (expression? incorrect)))
  (is (expression? (constant true)))
  (is (expression? (constant false)))
  (is (expression? (variable :x)))
  (is (expression? (And (variable :x) (variable :y))))
  (is (expression? (Or (variable :x) (variable :y))))
  (is (expression? (Not (variable :x))))
  (is (expression? (Impl (variable :x) (variable :x)))))
