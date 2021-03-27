(ns clojure-course.task4-1.cnf-test
  (:require [clojure.test :refer :all]
            [clojure-course.task4-1.cnf :refer :all]))

(def incorrect 0)

(deftest test-const
  (is (const? (const true)))
  (is (const? (const false)))
  (is (= true (const-value (const true))))
  (is (= false (const-value (const false))))
  (is (thrown? AssertionError (const incorrect)))
  (is (thrown? AssertionError (const? incorrect)))
  (is (thrown? AssertionError (const-value incorrect))))

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
  (is (impl? (impl (variable :x) (variable :y))))
  (is (thrown? AssertionError (impl incorrect incorrect)))
  (is (thrown? AssertionError (impl? incorrect))))

(deftest test-expr
  (is (not (expr? incorrect)))
  (is (expr? (const true)))
  (is (expr? (const false)))
  (is (expr? (variable :x)))
  (is (expr? (And (variable :x) (variable :y))))
  (is (expr? (Or (variable :x) (variable :y))))
  (is (expr? (Not (variable :x))))
  (is (expr? (impl (variable :x) (variable :x)))))

(deftest test-literal
  (is (literal? (variable :x)))
  (is (literal? (Not (variable :x))))
  (is (not (literal? (const true))))
  (is (not (literal? (And (variable :x) (variable :y)))))
  (is (not (literal? (Or (variable :x) (variable :y)))))
  (is (not (literal? (impl (variable :x) (variable :y)))))
  (is (thrown? AssertionError (literal? incorrect))))

(deftest test-elem-disj
  (is (elem-disj? (variable :x)))
  (is (elem-disj? (Not (variable :x))))
  (is (elem-disj? (Or (variable :x))))
  (is (elem-disj? (Or (Not (variable :x)))))
  (is (elem-disj? (Or (variable :x) (Not (variable :y)) (variable :z))))
  (is (not (elem-disj? (Or (And (variable :x) (Not (variable :y))) (variable :z)))))
  (is (not (elem-disj? (And (variable :x) (variable :y)))))
  (is (not (elem-disj? (impl (variable :x) (variable :y)))))
  (is (thrown? AssertionError (elem-disj? incorrect))))

(deftest test-cnf
  (is (cnf? (variable :x)))
  (is (cnf? (Not (variable :x))))
  (is (cnf? (And (variable :x) (Not (variable :y)) (variable :z))))
  (is (cnf? (Or (variable :x) (Not (variable :y)) (variable :z))))
  (is (cnf? (And (Or (variable :x) (variable :y)) (Or (variable :z) (variable :w))))) ; (x | y) & (z | w)
  (is (cnf? (And (variable :x) (Not (variable :y)) (Or (variable :z) (Not (variable :w)))))) ; x & !y & (z | !w)
  (is (not (cnf? (Or (variable :x) (And (variable :y) (variable :z)))))) ; x | (y & z)
  (is (not (cnf? (impl (variable :x) (variable :y)))))
  (is (thrown? AssertionError (cnf? incorrect))))

(deftest test-expr-as-str
  (is (= "true" (expr-as-str (const true))))
  (is (= "x" (expr-as-str (variable :x))))
  (is (= "!x" (expr-as-str (Not (variable :x)))))
  (is (= "(x | !y)" (expr-as-str (Or (variable :x) (Not (variable :y))))))
  (is (= "(x & !y & !(z | !w))" (expr-as-str (And (variable :x) (Not (variable :y)) (Not (Or (variable :z) (Not (variable :w))))))))
  (is (= "(x -> y)" (expr-as-str (impl (variable :x) (variable :y)))))
  (is (= "(x -> (y | z))" (expr-as-str (impl (variable :x) (Or (variable :y) (variable :z)))))))

(deftest test-impl-as-disj
  (is (= "(!x | y)" (expr-as-str (impl-as-disj (impl (variable :x) (variable :y))))))
  (-> (impl (variable :x) (variable :y))
      impl-as-disj
      expr-as-str
      (= "(!x | y)")
      is))
