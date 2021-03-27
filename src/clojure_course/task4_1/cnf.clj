(ns clojure-course.task4-1.cnf)

(defn expr?
  "Tests whether expr is a expression"
  [expr]
  {:post [(boolean? %)]}
  (and
    (coll? expr)
    (not (empty? expr))
    (keyword? (first expr))))

(defn args
  "List of expression's arguments"
  [expr]
  {:pre [(expr? expr)]}
  (rest expr))

(defn const
  "Creates a boolean constant"
  [bool]
  {:pre [(boolean? bool)]}
  (list ::const bool))

(defn const?
  "Tests whether expr is a constant"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (= (first expr) ::const))

(defn const-value
  "Extracts value from const"
  [const]
  {:pre  [(const? const)]
   :post [(boolean? %)]}
  (second const))

(defn variable
  "Creates a variable with name"
  [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable?
  "Tests whether expr is a variable"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (= (first expr) ::var))

(defn variable-name
  "Returns name of variable"
  [var]
  {:pre  [(variable? var)]
   :post [(keyword? %)]}
  (second var))

(defn same-variable?
  "Tests whether var1 and var2 are the same"
  [var1 var2]
  {:pre  [(expr? var1) (expr? var2)]
   :post [(boolean? %)]}
  (= (variable-name var1) (variable-name var2)))

(defn And
  "Creates a conjunction"
  [expr & rest]
  {:pre [(expr? expr)]}
  (if (empty? rest)
    expr
    (cons ::and (cons expr rest))))

(defn And?
  "Tests whether expr is a conjunction"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (= (first expr) ::and))

(defn Or
  "Creates a disjunction"
  [expr & rest]
  {:pre [(expr? expr)]}
  (if (empty? rest)
    expr
    (cons ::or (cons expr rest))))

(defn Or?
  "Tests whether expr is a disjunction"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (= (first expr) ::or))

(defn Not
  "Creates a negation"
  [expr]
  {:pre [(expr? expr)]}
  (cons ::not expr))

(defn Not?
  "Tests whether expr is a negation"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (= (first expr) ::not))

(defn impl
  "Creates a implication"
  [antecedent consequent]
  {:pre [(expr? antecedent) (expr? consequent)]}
  (list ::impl antecedent consequent))

(defn impl?
  "Tests whether expr is a implication"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (= (first expr) ::impl))

(defn literal?
  "Tests whether expr is a variable or its negation"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (or
    (variable? expr)
    (and
      (Not? expr)
      (expr? (args expr))
      (variable? (args expr)))))

(defn elem-disj?
  "Tests whether expr is a disjunction of literals"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (or
    (literal? expr)
    (and
      (Or? expr)
      (every? literal? (args expr)))))

(defn cnf?
  "Tests whether expr is in conjunctive normal form"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (or
    (elem-disj? expr)
    (and
      (And? expr)
      (every? elem-disj? (args expr)))))

(defn expr-str
  "Represents expr as a string"
  [expr]
  {:pre  [(expr? expr)]
   :post [(string? %)]}
  (cond
    (const? expr) (str (const-value expr))
    (variable? expr) (name (variable-name expr))
    (Not? expr) (str "!" (expr-str (args expr)))
    (Or? expr) (str "(" (clojure.string/join " | " (map #(expr-str %) (args expr))) ")")
    (And? expr) (str "(" (clojure.string/join " & " (map #(expr-str %) (args expr))) ")")
    (impl? expr) (str "(" (expr-str (first (args expr))) " -> " (expr-str (second (args expr))) ")")))

(defn impl-as-disj
  "Represents implication as disjunction: (x -> y) = (!x | y)"
  [expr]
  {:pre  [(impl? expr)]
   :post [(Or? %)]}
  (let [[x y] (args expr)]
    (Or (Not x) y)))

(defn de-morgan-disj
  "Represents negation of disjunction as conjunction of negations: !(x | y) = (!x & !y)"
  [expr]
  {:pre  [(Not? expr) (Or? (args expr))]
   :post [(And? %)]}
  (apply And (map #(Not %) (args (args expr)))))

(defn de-morgan-conj
  "Represents negation of conjunction as disjunction of negations: !(x & y) = (!x | !y)"
  [expr]
  {:pre  [(Not? expr) (And? (args expr))]
   :post [(Or? %)]}
  (apply Or (map #(Not %) (args (args expr)))))

(defn -main []
  ())
