(ns clojure-course.task6-1.dinner)

(def fork-count 5)
(def forks
  (map
    (fn [_] [(ref 0) (atom 0)])
    (range fork-count)))

(defn eat
  [l-fork r-fork think-time eat-time times]
  (fn []
    (reduce
      (fn [_ _]
        (Thread/sleep think-time)
        (dosync
          (swap! (last l-fork) inc)
          (swap! (last r-fork) inc)
          (alter (first l-fork) inc)
          (alter (first r-fork) inc)
          (Thread/sleep eat-time)))
      (range (inc times)))))

(defn dinner
  [think-time eat-time times]
  (let [fork-pairs
        (cons
          [(last forks) (first forks)]
          (map
            (fn [x y] [(nth forks x) (nth forks y)])
            (range (dec fork-count))
            (range 1 fork-count)))
        philosophers
        (doall
          (map
            (fn [[l-fork r-fork]]
              (new Thread (eat l-fork r-fork think-time eat-time times)))
            fork-pairs))]
    (doall (map #(.start %) philosophers))
    (doall (map #(.join %) philosophers))))

(defn -main []
  (println (map (fn [[r a]] [@r @a]) forks))
  (time (dinner 10 100 1))
  (println (map (fn [[r a]] [@r @a]) forks)))
