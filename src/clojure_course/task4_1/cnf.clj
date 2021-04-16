(ns clojure-course.task4-1.cnf)

(defn expr?
  "Tests whether expr is an expression"
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

(defn atom?
  "Tests whether expr is a constant or a literal"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (or
    (const? expr)
    (literal? expr)))

(defn not-atom?
  "Tests whether expr is not a atom"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (not (atom? expr)))

(defn elementary?
  [expr pred]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (or
    (atom? expr)
    (and
      (pred expr)
      (every? atom? (args expr)))))

(defn elem-disj?
  "Tests whether expr is a disjunction of atoms"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (elementary? expr Or?))

(defn elem-conj?
  "Tests whether expr is a conjunction of atoms"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (elementary? expr And?))

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

(defn not-cnf?
  "Tests whether expr is in conjunctive normal form"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (not (cnf? expr)))

(defn impless?
  "Tests whether expr does not contain an implication"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (cond
    (atom? expr) true
    (impl? expr) false
    (Not? expr) (impless? (args expr))
    :default (every? impless? (args expr))))

(defn not-impless?
  "Tests whether expr does not contain an implication"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (not (impless? expr)))

(defn nnf?
  "Tests whether expr is in negation normal form"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (cond
    (and (Not? expr) (not-atom? expr)) false
    (atom? expr) true
    :default (every? nnf? (args expr))))

(defn not-nnf?
  "Tests whether expr is not in negation normal form"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (not (nnf? expr)))

(defn type-and-inner-type?
  [expr outer-type inner-type]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (and
    (outer-type expr)
    (some? (some inner-type (args expr)))))

(defn assoc-disj?
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (type-and-inner-type? expr Or? Or?))

(defn assoc-conj?
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (type-and-inner-type? expr And? And?))

(defn distributive?
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (type-and-inner-type? expr Or? And?))

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

(defn flatten-by-pred
  [pred expr]
  {:pre [(or
           (= pred Or?)
           (= pred And?))
         (expr? expr)]}
  (if (pred expr)
    (mapcat #(flatten-by-pred pred %) (args expr))
    (list expr)))

(defn assoc-disj-law
  "Allows removal of brackets from disjunction: (x | (y | z)) = (x | y | z)"
  [expr]
  {:pre  [(Or? expr)]
   :post [(Or? %)]}
  (apply Or (flatten-by-pred Or? expr)))

(defn assoc-conj-law
  "Allows removal of brackets from conjunction: (x & (y & z)) = (x & y & z)"
  [expr]
  {:pre  [(And? expr)]
   :post [(And? %)]}
  (apply And (flatten-by-pred And? expr)))

(defn not-and?
  "Tests whether expr os not and operation"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (not (And? expr)))

(defn distr-pairs
  [expr]
  {:pre [(Or? expr)]}
  (let [args-part1 (take-while not-and? (args expr))
        args-part2 (drop-while not-and? (args expr))
        first-and (first args-part2)]
    (for [args-and (args first-and)
          args-or (concat args-part1 (rest args-part2))]
      [args-or args-and])))

(defn apply-rules
  [expr rules]
  {:pre  [(expr? expr) (list? rules)]
   :post [(expr? %)]}
  (let [chosen-rule
        (some
          (fn [rule]
            (if ((first rule) expr)
              (second rule)
              false))
          rules)]
    ;(println (str "Applying: " chosen-rule))
    ;(println (str " to " expr))
    (chosen-rule expr)))

(declare impless)

(defn impl-to-disj
  "Represents implication as disjunction: (x -> y) = (!x | y)"
  [expr]
  {:pre  [(impl? expr)]
   :post [(Or? %)]}
  (let [[x y] (args expr)]
    (Or (Not (impless x)) (impless y))))

(def impless-rules
  (list
    [atom? identity]
    [impl? impl-to-disj]
    [Or? #(apply Or (map impless (args %)))]
    [And? #(apply And (map impless (args %)))]
    [Not? #(Not (impless (args %)))]))

(defn impless
  "Replaces all implications with disjunctions"
  [expr]
  {:pre  [(expr? expr)]
   :post [(expr? %)]}
  (apply-rules expr impless-rules))

(declare nnf)

(defn const-negate
  "Negates boolean const"
  [expr]
  {:pre  [(const? expr)]
   :post [(const? %)]}
  (if (const-value expr)
    (const false)
    (const true)))

(defn const-negate?
  "Tests whether expr is a negation of boolean constant"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (and
    (Not? expr)
    (const? (args expr))))

(defn double-not?
  "Tests whether expr is a negation of negation"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (and
    (Not? expr)
    (Not? (args expr))))

(defn de-morgan-disj?
  "Tests whether expr is a negation of disjunction"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (and
    (Not? expr)
    (Or? (args expr))))

(defn de-morgan-conj?
  "Tests whether expr is a negation of conjunction"
  [expr]
  {:pre  [(expr? expr)]
   :post [(boolean? %)]}
  (and
    (Not? expr)
    (And? (args expr))))

(defn apply-op
  "Applies operation to expr args"
  [op map-fn expr-args]
  (apply op (map map-fn expr-args)))

(defn apply-or
  "Applies or operation to expr args"
  [map-fn expr-args]
  (apply-op Or map-fn expr-args))

(defn apply-and
  "Applies and operation to expr args"
  [map-fn expr-args]
  (apply-op And map-fn expr-args))

(defn args-args
  [expr]
  {:pre  [(expr? expr)]
   :post [(or (expr? %) (every? expr? %))]}
  (args (args expr)))

(def nnf-rules
  (list
    [not-impless? #(nnf (impless %))]
    [atom? identity]
    [const-negate? #(const-negate (args %))]
    [double-not? #(nnf (args-args %))]
    [de-morgan-disj? (fn [expr] (apply-and #(nnf (Not %)) (args-args expr)))]
    [de-morgan-conj? (fn [expr] (apply-or #(nnf (Not %)) (args-args expr)))]
    [And? #(apply-and nnf (args %))]
    [Or? #(apply-or nnf (args %))]
    ))

(defn nnf
  "Represents expr as negation normal form"
  [expr]
  {:pre  [(expr? expr)]
   :post [(expr? %)]}
  (apply-rules expr nnf-rules))

(declare cnf)

(defn contains-const?
  "Tests whether expr contains a constant"
  ([expr]
   {:pre  [(expr? expr)]
    :post [(boolean? %)]}
   (cond
     (const? expr) true
     (literal? expr) false
     (Not? expr) (const? (args expr))
     (Or? expr) (some? (some const? (args expr)))
     (And? expr) (some? (some const? (args expr)))
     (impl? expr) (some? (some const? (args expr)))))
  ([expr constant]
   {:pre  [(expr? expr) (const? constant)]
    :post [(boolean? %)]}
   (cond
     (const? expr) (= expr constant)
     (literal? expr) false
     (Not? expr) (= (args expr) constant)
     (Or? expr) (some? (some #(= % constant) (args expr)))
     (And? expr) (some? (some #(= % constant) (args expr)))
     (impl? expr) (some? (some #(= % constant) (args expr))))))

(defn identity-law
  [expr]
  {:pre  [(expr? expr)]
   :post [(expr? %)]}
  (cond
    (and (And? expr) (contains-const? expr (const false))) (const false)
    (and (Or? expr) (contains-const? expr (const true))) (const true)
    (and (And? expr) (contains-const? expr (const true))) (apply And (filter #(not (= % (const true))) (args expr)))
    (and (Or? expr) (contains-const? expr (const false))) (apply Or (filter #(not (= % (const false))) (args expr)))
    :else expr))

(defn idempotent-disj-law
  "(x | y | x) = (x | y)"
  [expr]
  {:pre  [(Or? expr)]
   :post [(Or? %)]}
  (apply Or (distinct (args expr))))

(defn idempotent-conj-law
  "(x & y & x) = (x & y)"
  [expr]
  {:pre  [(And? expr)]
   :post [(And? %)]}
  (apply And (distinct (args expr))))

(def cnf-rules
  (list
    [not-nnf? #(cnf (nnf %))]
    [atom? identity]
    [contains-const? #(cnf (identity-law %))]
    [elem-disj? idempotent-disj-law]
    [elem-conj? idempotent-conj-law]
    [assoc-disj? #(cnf (idempotent-disj-law (assoc-disj-law %)))]
    [assoc-conj? #(cnf (idempotent-conj-law (assoc-conj-law %)))]
    [distributive?
     #(assoc-conj-law
        (apply-and
          (fn [[x y]] (cnf (assoc-disj-law (Or x y))))
          (distr-pairs %)))]
    [And? #(apply-and cnf (args %))]))

(defn cnf
  "Represents expr as conjunction normal form"
  [expr]
  {:pre  [(expr? expr)]
   :post [(expr? %)]}
  (let [res (apply-rules expr cnf-rules)]
    (if (and (not (const? res)) (contains-const? res))
      (cnf res)
      res)))

(defn recursive-assign
  "Assigns const value to variable in expr"
  [expr var val]
  {:pre  [(expr? expr) (variable? var) (const? val)]
   :post [(expr? %)]}
  (cond
    (and (variable? expr) (= expr var)) val
    (Not? expr) (Not (recursive-assign (args expr) var val))
    (And? expr) (apply And (map #(recursive-assign % var val) (args expr)))
    (Or? expr) (apply Or (map #(recursive-assign % var val) (args expr)))
    (impl? expr) (impl (recursive-assign (first (args expr)) var val) (recursive-assign (second (args expr)) var val))
    :else expr))

(defn assign
  "Assigns const value to variable in expr"
  [expr var val]
  {:pre  [(expr? expr) (variable? var) (const? val)]
   :post [(expr? %)]}
  (cnf (recursive-assign expr var val)))
