(ns clojure-course.task4-1.cnf)

(defn constant
  "Creates a boolean constant"
  [bool]
  {:pre [(boolean? bool)]}
  (list ::const bool))

(defn constant?
  "Tests whether expr is a constant"
  [expr]
  {:pre [(and (list? expr) (= (count expr) 2))]}
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
  {:pre [(and (list? expr) (= (count expr) 2))]}
  (= (first expr)) ::var)

(defn variable-name
  "Returns name of variable"
  [var]
  {:pre [(variable? var)]}
  (second var))

(defn same-variable?
  "Tests whether var1 and var2 are the same"
  [var1 var2]
  {:pre [(variable? var1) (variable? var2)]}
  (= (variable-name var1) (variable-name var2)))
