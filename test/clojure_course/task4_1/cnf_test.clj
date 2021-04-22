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

(deftest test-atom
  (is (atom? (variable :x)))
  (is (atom? (Not (variable :x))))
  (is (atom? (const true)))
  (is (not (atom? (And (variable :x) (variable :y)))))
  (is (not (atom? (Or (variable :x) (variable :y)))))
  (is (not (atom? (impl (variable :x) (variable :y)))))
  (is (thrown? AssertionError (atom? incorrect))))

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

(deftest test-elem-conj
  (is (elem-conj? (variable :x)))
  (is (elem-conj? (Not (variable :x))))
  (is (elem-conj? (And (variable :x))))
  (is (elem-conj? (And (Not (variable :x)))))
  (is (elem-conj? (And (variable :x) (Not (variable :y)) (variable :z))))
  (is (not (elem-conj? (And (Or (variable :x) (Not (variable :y))) (variable :z)))))
  (is (not (elem-conj? (Or (variable :x) (variable :y)))))
  (is (not (elem-conj? (impl (variable :x) (variable :y)))))
  (is (thrown? AssertionError (elem-conj? incorrect))))

(deftest test-contains-const
  (is (contains-const? (const false)))
  (is (contains-const? (Not (const false))))
  (is (contains-const? (And (const false) (variable :x))))
  (is (contains-const? (Or (const false) (variable :z))))
  (is (contains-const? (impl (const false) (impl (And (variable :x) (variable :y)) (Or (variable :z) (variable :w))))))
  (is (not (contains-const? (variable :x))))
  (is (not (contains-const? (Not (variable :x)))))
  (is (not (contains-const? (And (Not (variable :x)) (variable :y)))))
  (is (not (contains-const? (Or (And (variable :x) (Not (variable :y))) (variable :z)))))
  (is (not (contains-const? (impl (Not (variable :m)) (impl (And (variable :x) (variable :y)) (Or (variable :z) (variable :w)))))))

  (is (contains-const? (const false) (const false)))
  (is (not (contains-const? (const false) (const true))))
  (is (contains-const? (Not (const false)) (const false)))
  (is (not (contains-const? (Not (const false)) (const true))))
  (is (contains-const? (And (const false) (variable :x)) (const false)))
  (is (not (contains-const? (And (const false) (variable :x)) (const true))))
  (is (contains-const? (Or (const false) (variable :z)) (const false)))
  (is (not (contains-const? (Or (const false) (variable :z)) (const true))))
  (is (contains-const? (impl (const false) (impl (And (variable :x) (variable :y)) (Or (variable :z) (variable :w)))) (const false)))
  (is (not (contains-const? (impl (const false) (impl (And (variable :x) (variable :y)) (Or (variable :z) (variable :w)))) (const true))))
  (is (thrown? AssertionError (contains-const? incorrect))))

(deftest test-cnf
  (is (cnf? (variable :x)))
  (is (cnf? (Not (variable :x))))
  (is (cnf? (And (variable :x) (Not (variable :y)) (variable :z))))
  (is (cnf? (Or (variable :x) (Not (variable :y)) (variable :z))))
  (is (cnf? (And (Or (variable :x) (variable :y)) (Or (variable :z) (variable :w)))))
  (is (cnf? (And (variable :x) (Not (variable :y)) (Or (variable :z) (Not (variable :w))))))
  (is (not-cnf? (Or (variable :x) (And (variable :y) (variable :z)))))
  (is (not-cnf? (impl (variable :x) (variable :y))))
  (is (thrown? AssertionError (cnf? incorrect)))
  (is (thrown? AssertionError (not-cnf? incorrect))))

(deftest test-assoc-disj
  (is (assoc-disj? (Or (variable :x) (Or (variable :y) (variable :z)))))
  (is (not (assoc-disj? (Or (variable :x) (variable :y)))))
  (is (thrown? AssertionError (assoc-disj? incorrect))))

(deftest test-assoc-conj
  (is (assoc-conj? (And (variable :x) (And (variable :y) (variable :z)))))
  (is (not (assoc-conj? (And (variable :x) (variable :y)))))
  (is (thrown? AssertionError (assoc-conj? incorrect))))

(deftest test-expr-as-str
  (is (= "true" (expr-str (const true))))
  (is (= "x" (expr-str (variable :x))))
  (is (= "!x" (expr-str (Not (variable :x)))))
  (is (= "(x | !y)" (expr-str (Or (variable :x) (Not (variable :y))))))
  (is (= "(x & !y & !(z | !w))" (expr-str (And (variable :x) (Not (variable :y)) (Not (Or (variable :z) (Not (variable :w))))))))
  (is (= "(x -> y)" (expr-str (impl (variable :x) (variable :y)))))
  (is (= "(x -> (y | z))" (expr-str (impl (variable :x) (Or (variable :y) (variable :z)))))))

(deftest test-assoc-disj-law
  (is (= "(x | y | (z & w) | v | (m -> n))" (expr-str (assoc-disj-law (Or (Or (variable :x) (variable :y) (And (variable :z) (variable :w))) (variable :v) (impl (variable :m) (variable :n))))))))

(deftest test-assoc-conj-law
  (is (= "(x & y & (z | w) & v & (m -> n))" (expr-str (assoc-conj-law (And (And (variable :x) (variable :y) (Or (variable :z) (variable :w))) (variable :v) (impl (variable :m) (variable :n))))))))

(deftest test-idempotent-disj-law
  (is (= "(x | y | (z & m))" (expr-str (idempotent-disj-law (Or (variable :x) (variable :y) (variable :x) (And (variable :z) (variable :m)) (And (variable :z) (variable :m))))))))

(deftest test-idempotent-conj-law
  (is (= "(x & y & (z | m))" (expr-str (idempotent-conj-law (And (variable :x) (variable :y) (variable :x) (Or (variable :z) (variable :m)) (Or (variable :z) (variable :m))))))))

(deftest test-impless
  (is (= "(!x | y)" (expr-str (impless (impl (variable :x) (variable :y))))))
  (is (= "(!(x | !y) | (z & !w))" (expr-str (impless (impl (Or (variable :x) (Not (variable :y))) (And (variable :z) (Not (variable :w))))))))
  (is (= "(!!true | (!(x & y) | (z | w)))" (expr-str (impless (impl (Not (const true)) (impl (And (variable :x) (variable :y)) (Or (variable :z) (variable :w))))))))
  (is (impless? (impless (impl (variable :x) (variable :y)))))
  (is (impless? (impless (impl (Or (variable :x) (Not (variable :y))) (And (variable :z) (Not (variable :w)))))))
  (is (impless? (impless (impl (Not (const true)) (impl (And (variable :x) (variable :y)) (Or (variable :z) (variable :w)))))))
  (is (not-impless? (impl (variable :x) (variable :y))))
  (is (not-impless? (impl (Or (variable :x) (Not (variable :y))) (And (variable :z) (Not (variable :w))))))
  (is (not-impless? (impl (Not (const true)) (impl (And (variable :x) (variable :y)) (Or (variable :z) (variable :w))))))
  (is (thrown? AssertionError (impless? incorrect))))

(deftest test-const-negate
  (is (const-value (const-negate (const false))))
  (is (not (const-value (const-negate (const true)))))
  (is (const-negate? (Not (const false))))
  (is (not (const-negate? (const false))))
  (is (thrown? AssertionError (const-negate incorrect)))
  (is (thrown? AssertionError (const-negate? incorrect))))

(deftest test-double-not
  (is (double-not? (Not (Not (variable :x)))))
  (is (not (double-not? (Not (variable :x)))))
  (is (thrown? AssertionError (double-not? incorrect))))

(deftest test-nnf
  (is (= "x" (expr-str (nnf (Not (Not (variable :x)))))))
  (is (= "(x & y)" (expr-str (nnf (Not (Not (And (variable :x) (variable :y))))))))
  (is (= "(false | (x & y))" (expr-str (nnf (Or (Not (Not (Not (const true)))) (Not (Not (And (variable :x) (variable :y)))))))))
  (is (nnf? (nnf (Not (Not (variable :x))))))
  (is (nnf? (nnf (Not (Not (Not (const true)))))))
  (is (nnf? (nnf (Not (Not (And (variable :x) (variable :y)))))))
  (is (nnf? (nnf (Or (Not (Not (Not (const true)))) (Not (Not (And (variable :x) (variable :y))))))))
  (is (nnf? (nnf (Not (impl (Not (const false)) (impl (And (variable :x) (variable :y)) (Or (And (variable :z) (variable :r)) (variable :w))))))))
  (is (not-nnf? (Not (Not (variable :x)))))
  (is (not-nnf? (Not (Not (Not (const true))))))
  (is (not-nnf? (Not (Not (And (variable :x) (variable :y))))))
  (is (not-nnf? (Or (Not (Not (Not (const true)))) (Not (Not (And (variable :x) (variable :y)))))))
  (is (thrown? AssertionError (nnf incorrect)))
  (is (thrown? AssertionError (nnf? incorrect)))
  (is (thrown? AssertionError (not-nnf? incorrect))))

(deftest test-to-cnf
  (is (= "(x & y)" (expr-str (cnf (And (variable :x) (variable :y))))))
  (is (= "((x | z) & (x | m))" (expr-str (cnf (Or (variable :x) (And (variable :z) (variable :m)))))))
  (is (= "((x | z) & (x | m) & (y | z) & (y | m))" (expr-str (cnf (Or (And (variable :x) (variable :y)) (And (variable :z) (variable :m)))))))
  (is (= "(x & y & (!z | !r) & !w)" (expr-str (cnf (Not (impl (Not (const false)) (impl (And (variable :x) (variable :y)) (Or (And (variable :z) (variable :r)) (variable :w)))))))))
  (is (= "false" (expr-str (cnf (Not (impl (Not (const true)) (impl (And (variable :x) (variable :y)) (Or (And (variable :z) (variable :r)) (variable :w)))))))))
  (is (cnf? (cnf (And (variable :x) (variable :y)))))
  (is (cnf? (cnf (Or (variable :x) (And (variable :z) (variable :m))))))
  (is (cnf? (cnf (Or (And (variable :x) (variable :y)) (And (variable :z) (variable :m))))))
  (is (cnf? (cnf (Not (impl (Not (const false)) (impl (And (variable :x) (variable :y)) (Or (variable :z) (variable :w))))))))
  (is (= "(x | y | z)" (expr-str (cnf (Or (variable :x) (variable :y) (Or (variable :x) (variable :z)))))))
  (is (= "(x | y)" (expr-str (cnf (Or (variable :x) (variable :y) (variable :x))))))
  (is (= "true" (expr-str (cnf (Or (variable :x) (Not (variable :x)))))))
  (is (= "false" (expr-str (cnf (And (variable :x) (Not (variable :x))))))))

(deftest test-assign
  (is (= "y" (expr-str (assign (And (variable :x) (variable :y)) (variable :x) (const true)))))
  (is (= "false" (expr-str (assign (And (variable :x) (variable :y)) (variable :x) (const false)))))
  (is (= "true" (expr-str (assign (Or (Not (variable :x)) (Not (variable :y))) (variable :x) (const false)))))
  (is (= "(x & y & !r & !w)" (expr-str (assign (Not (impl (Not (const false)) (impl (And (variable :x) (variable :y)) (Or (And (variable :z) (variable :r)) (variable :w))))) (variable :z) (const true)))))
  (is (= "(x & y & !w)" (expr-str (assign (Not (impl (Not (const false)) (impl (And (variable :x) (variable :y)) (Or (And (variable :z) (variable :r)) (variable :w))))) (variable :z) (const false))))))

(deftest test-complement-conj-law
  (is (= "false" (expr-str (complement-conj-law (And (variable :x) (Not (variable :x)))))))
  (is (= "(x & !y)" (expr-str (complement-conj-law (And (variable :x) (Not (variable :y)))))))
  (is (= "(x & y)" (expr-str (complement-conj-law (And (variable :x) (variable :y)))))))

(deftest test-complement-disj-law
  (is (= "true" (expr-str (complement-disj-law (Or (variable :x) (Not (variable :x)))))))
  (is (= "(x | !y)" (expr-str (complement-disj-law (Or (variable :x) (Not (variable :y)))))))
  (is (= "(x | y)" (expr-str (complement-disj-law (Or (variable :x) (variable :y)))))))
