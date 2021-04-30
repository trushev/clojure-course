(ns clojure-course.task6-1.dinner)

(defn- forks
  [fork-count]
  (map
    (fn [_] [(ref 0) (atom 0)])
    (range fork-count)))

(defn- print-forks
  [forks]
  (println (map (fn [[r a]] [@r @a]) forks))
  forks)

(defn- fork-pairs
  [forks]
  (cons
    [(last forks) (first forks)]
    (map
      (fn [x y] [(nth forks x) (nth forks y)])
      (range (dec (count forks)))
      (range 1 (count forks)))))

(defn- eat
  [l-fork r-fork think-time eat-time piece-count]
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
      (range (inc piece-count)))))

(defn- philosophers
  [fork-pairs think-time eat-time piece-count]
  (doall
    (map
      (fn [[l-fork r-fork]]
        (new
          Thread
          (eat l-fork r-fork think-time eat-time piece-count)))
      fork-pairs)))

(defn dinner
  [fork-count think-time eat-time piece-count]
  (let [forks (print-forks (forks fork-count))
        philosophers (philosophers
                       (fork-pairs forks)
                       think-time
                       eat-time
                       piece-count)]
    (doall (map #(.start %) philosophers))
    (doall (map #(.join %) philosophers))
    forks))

(def ^:private fork-count identity)
(def ^:private think-time identity)
(def ^:private eat-time identity)
(def ^:private piece-count identity)

(defn -main []
  (print-forks
    (time
      (dinner
        (fork-count 5)
        (think-time 10)
        (eat-time 100)
        (piece-count 1)))))
