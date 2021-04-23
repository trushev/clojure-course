(ns clojure-course.task5-2.lazy-parallel-filter)

(def parallelism 4)

(defn- split-coll
  [coll block-size res]
  (if (empty? coll)
    (reverse res)
    (recur
      (drop block-size coll)
      block-size
      (cons (take block-size coll) res))))

(defn- blocks
  [coll block-count]
  (split-coll
    coll
    (/ (count coll) block-count)
    (list)))

(defn- pfilter
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

(defn- chunks
  [coll chunk-size]
  (take-while
    #(not (empty? %))
    (map
      #(take chunk-size %)
      (iterate
        #(drop chunk-size %)
        coll))))

(def block-size 100)

(defn lazy-pfilter
  ([pred coll] (lazy-pfilter pred coll parallelism block-size))
  ([pred coll par] (lazy-pfilter pred coll par block-size))
  ([pred coll par bsize]
   (mapcat
     #(pfilter pred % par)
     (chunks coll (* par bsize)))))
