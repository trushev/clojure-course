(ns clojure-course.task5-1.parallel-filter-test
  (:require [clojure.test :refer :all]
            [clojure-course.task5-1.parallel-filter :refer :all]))

(defn same-values?
  [pred coll]
  (=
    (set (filter pred coll))
    (set (pfilter pred coll))))

(deftest pfilter-value-test
  (is (same-values? even? (range 100)))
  (is (same-values? even? (range 99)))
  (is (same-values? even? (range 98)))
  (is (same-values? even? (range 97)))
  (is (same-values? even? (range 96))))

(defn long-even? [n]
  (Thread/sleep 10)
  (even? n))

(defn compare-time
  [pred coll]
  (println "Filter time:")
  (time (doall (filter pred coll)))
  (println "PFilter time:")
  (time (doall (pfilter pred coll))))

(deftest pfilter-time-test
  (compare-time long-even? (range 100)))
