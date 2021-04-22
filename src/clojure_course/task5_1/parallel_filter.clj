(ns clojure-course.task5-1.parallel-filter)

(def parallelism 4)

(defn- split-coll
  [coll block-size res]
  (if (empty? coll)
    res
    (split-coll
      (drop block-size coll)
      block-size
      (cons (take block-size coll) res))))

(defn- blocks
  [coll block-count]
  (split-coll
    coll
    (/ (count coll) block-count)
    (list)))

(defn pfilter
  ([pred coll] (pfilter pred coll parallelism))
  ([pred coll par]
   (apply
     concat
     (map
       deref
       (doall
         (map
           #(future (doall (filter pred %)))
           (blocks coll par)))))))

(defn -main []
  (println (blocks (list 1 2 3 4 5 6 7 8 9 10) parallelism)))
