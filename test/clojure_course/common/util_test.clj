(ns clojure-course.common.util-test
  (:require [clojure.test :refer :all]))

(defn to-set [a-list]
  (apply hash-set a-list))

(defn base-test-wfa [wfa-impl]
  (is
    (=
     #{"cb" "ca" "bc" "ba" "ac" "ab"}
     (to-set (wfa-impl `("a" "b" "c") 2))))
  (is
    (=
     #{"aba" "bab"}
     (to-set (wfa-impl `("a" "b") 3))))
  (is
    (=
     #{}
     (to-set (wfa-impl `("a") 0)))))
