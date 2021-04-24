(ns clojure-course.task6-1.dinner)

(def fork-count 5)
(def forks (map (fn [_] [(ref 0) (atom 0)]) (range fork-count)))
(def fork-pairs
  (cons
    [(last forks) (first forks)]
    (map
      (fn [x y] [(nth forks x) (nth forks y)])
      (range (dec fork-count))
      (range 1 fork-count))))

(defn eat
  [l-fork r-fork eat-time]
  (fn []
    (dosync
      (swap! (last l-fork) inc)
      (swap! (last r-fork) inc)
      (alter (first l-fork) inc)
      (alter (first r-fork) inc)
      (Thread/sleep eat-time))))

(defn start
  [eat-time]
  (let [philosophers
        (doall
          (map
            (fn [[l-fork r-fork]]
              (new Thread (eat l-fork r-fork eat-time)))
            fork-pairs))]
    (doall (map #(.start %) philosophers))
    (doall (map #(.join %) philosophers))))

(defn -main []
  (println (map (fn [[r a]] [@r @a]) forks))
  (time (start 100))
  (println (map (fn [[r a]] [@r @a]) forks))
  )
