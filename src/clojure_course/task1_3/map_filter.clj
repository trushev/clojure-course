(ns clojure-course.task1-3.map-filter)

(defn my-map [f coll]
  (apply
   list
   (reduce
    (fn [acc val] (conj acc (f val)))
    []
    coll)))

(defn my-filter [pred coll]
  (apply
   list
   (reduce
    (fn [acc val]
      (if (pred val)
        (conj acc val)
        acc))
    []
    coll)))
