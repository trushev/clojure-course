(ns clojure-course.task4-1.cnf)

(defn expression?
  "Tests whether expr is a expression"
  [expr]
  (and
    (coll? expr)
    (not (empty? expr))
    (keyword? (first expr))))

(defn constant
  "Creates a boolean constant"
  [bool]
  {:pre [(boolean? bool)]}
  (list ::const bool))

(defn constant?
  "Tests whether expr is a constant"
  [expr]
  {:pre [(expression? expr)]}
  (= (first expr)) ::const)

(defn constant-value
  "Extracts value from const"
  [const]
  {:pre [(constant? const)]}
  (second const))

(defn variable
  "Creates a variable with name"
  [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable?
  "Tests whether expr is a variable"
  [expr]
  {:pre [(expression? expr)]}
  (= (first expr)) ::var)

(defn variable-name
  "Returns name of variable"
  [var]
  {:pre [(variable? var)]}
  (second var))

(defn same-variable?
  "Tests whether var1 and var2 are the same"
  [var1 var2]
  {:pre [(expression? var1) (expression? var2)]}
  (= (variable-name var1) (variable-name var2)))

(defn And
  "Creates a conjunction"
  [expr & rest]
  {:pre [(expression? expr)]}
  (if (empty? rest)
    expr
    (cons ::and (cons expr rest))))

(defn And?
  "Tests whether expr is a conjunction"
  [expr]
  {:pre [(expression? expr)]}
  (= (first expr) ::and))

(defn Or
  "Creates a disjunction"
  [expr & rest]
  {:pre [(expression? expr)]}
  (if (empty? rest)
    expr
    (cons ::or (cons expr rest))))

(defn Or?
  "Tests whether expr is a disjunction"
  [expr]
  {:pre [(expression? expr)]}
  (= (first expr) ::or))

(defn Not
  "Creates a negation"
  [expr]
  {:pre [(expression? expr)]}
  (list ::not expr))

(defn Not?
  "Tests whether expr is a negation"
  [expr]
  {:pre [(expression? expr)]}
  (= (first expr) ::not))

(defn Impl
  "Creates a implication"
  [expr-x expr-y]
  {:pre [(expression? expr-x) (expression? expr-y)]}
  (list ::impl expr-x expr-y))

(defn Impl?
  "Tests whether expr is a implication"
  [expr]
  {:pre [(expression? expr)]}
  (= (first expr) ::impl))

(defn -main []
  (println (And (variable :x) (variable :y))))
