(ns clojure-course.task5-2.lazy-parallel-filter-test
  (:require [clojure.test :refer :all]
            [clojure-course.task5-2.lazy-parallel-filter :refer :all]))

(def naturals
  (lazy-seq
    (cons 1 (map inc naturals))))

(deftest lazy-pfilter-lazy-seq-test
  (let [parallelism 2
        block-size 5
        chunk-count 10
        size (* chunk-count parallelism block-size)]
    (is (=
          (set (take size (filter even? naturals)))
          (set (take size (lazy-pfilter even? naturals parallelism block-size)))))))

(defn same-values?
  [pred coll]
  (=
    (set (filter pred coll))
    (set (lazy-pfilter pred coll))))

(deftest lazy-pfilter-value-test
  (is (same-values? even? (range 1000)))
  (is (same-values? even? (range 9999)))
  (is (same-values? even? (range 998)))
  (is (same-values? even? (range 997)))
  (is (same-values? even? (range 996)))
  (is (same-values? even? (range 995)))
  (is (same-values? even? (range 994))))

(defn long-even? [n]
  (Thread/sleep 1)
  (even? n))

(defn compare-time
  [pred coll]
  (println "Filter time:")
  (time (doall (filter pred coll)))
  (println "LazyPFilter time:")
  (time (doall (lazy-pfilter pred coll))))

(deftest pfilter-time-test
  (compare-time long-even? (range 1000)))
