(ns clojure-course.task7-1.air_ticket_util)

(defn- ticket-str
  [ticket]
  (str "{price: " (get ticket :price) ", tickets: " @(get ticket :tickets) "}"))

(defn- ticket-map-str
  [ticket-map]
  (let [s
        (reduce
          (fn [acc val]
            (str acc (str (first val) ": " (ticket-str (second val)) ", ")))
          ""
          ticket-map)]
    (subs s 0 (- (count s) 2))))

(defn- print-routes
  [routes]
  (reduce
    (fn [_ val]
      (println (str "    " (first val) ": {" (ticket-map-str (second val)) "},")))
    ""
    routes))

(defn- print-route-map
  [route-map]
  (println "{")
  (println "  forward: {")
  (print-routes (get route-map :forward))
  (println "  },")
  (println "  backward: {")
  (print-routes (get route-map :backward))
  (println "}"))

(defn- get-ticket
  [route-map from to]
  (let [routes (get (get route-map :forward) from)]
    (if (not (nil? routes))
      (let [route (get routes to)]
        (if (not (nil? route))
          (let [tickets (get route :tickets)]
            (if (> @tickets 0)
              tickets)))))))

(defn- has-ticket?
  ([ticket]
   (not (nil? ticket)))
  ([route-map from to]
   (has-ticket?
     (get-ticket route-map from to))))

(defn- sort-map
  [a-map]
  (sort-by last a-map))

(defn- not-visited
  [graph]
  (let [vm (first (sort-map (get graph :not-visited)))]
    (if (not (nil? vm))
      (first vm))))

(defn- neighbors
  [graph vertex]
  (map first (sort-map (get (get graph :routes) vertex))))

(defn- vertex-status
  [graph vertex]
  (if (nil? (get (get graph :not-visited) vertex))
    :visited
    :not-visited))

(defn- updated-mark
  [graph vertex-status vertex vertex-mark]
  (assoc
    graph
    vertex-status
    (assoc (get graph vertex-status) vertex vertex-mark)))

(defn- updated-path
  [graph source dest]
  (assoc
    graph
    :paths
    (assoc (get graph :paths) dest (conj (get (get graph :paths) source) dest))))

(defn- mark
  [graph source dest]
  (let [source-mark (get (get graph :not-visited) source)
        dest-status (vertex-status graph dest)
        dest-mark (get (get graph dest-status) dest)
        route (get (get (get graph :routes) source) dest)
        new-dest-mark (+ source-mark route)
        need-update? (> dest-mark new-dest-mark)]
    (if need-update?
      (updated-path
        (updated-mark
          graph
          dest-status
          dest
          new-dest-mark)
        source
        dest)
      graph)))

(defn- remove-vertex
  [graph vertex]
  (assoc
    graph
    :not-visited
    (dissoc (get graph :not-visited) vertex)))

(defn- visit
  [graph vertex]
  (let [vertex-mark (get (get graph :not-visited) vertex)
        updated-graph (remove-vertex graph vertex)]
    (assoc
      updated-graph
      :visited
      (assoc (get updated-graph :visited) vertex vertex-mark))))

(defn dijkstras-algorithm
  [graph]
  (let [not-visited (not-visited graph)]
    (if (nil? not-visited)
      graph
      (recur
        (visit
          (reduce
            (fn [updated-graph neighbor]
              (mark updated-graph not-visited neighbor))
            graph
            (neighbors graph not-visited))
          not-visited)))))

(defn- init-vertex-routes
  [vertex-routes]
  (reduce
    (fn [acc val] (assoc acc (first val) (second val)))
    {}
    (map
      (fn [[v p _]] (list v p))
      (filter
        (fn [[_ _ t]] (> t 0))
        (map
          (fn [x]
            (list
              (first x)
              (get (second x) :price)
              @(get (second x) :tickets)))
          vertex-routes)))))

(defn- init-routes
  [route-map routes]
  (reduce
    (fn [updated-routes vertex-routes]
      (assoc
        updated-routes
        (first vertex-routes)
        (init-vertex-routes (second vertex-routes))))
    routes
    (get route-map :forward)))

(defn init-graph
  [route-map start-vertex]
  (let [not-visited
        (assoc
          (reduce
            (fn [acc val] (assoc acc val ##Inf))
            {}
            (concat
              (keys (get route-map :forward))
              (keys (get route-map :backward))))
          start-vertex
          0)
        visited {}
        paths {start-vertex [start-vertex]}
        routes
        (init-routes
          route-map
          (reduce
            (fn [acc val] (assoc acc val {}))
            {}
            (keys not-visited)))]
    (-> {}
        (assoc :not-visited not-visited)
        (assoc :visited visited)
        (assoc :paths paths)
        (assoc :routes routes))))

(defn found-path
  [graph end]
  (if (= (get (get graph :visited) end) ##Inf)
    (list 0 (list))
    (list
      (get (get graph :visited) end)
      (let [path (get (get graph :paths) end)]
        (map
          (fn [x y] [x y])
          (reverse (rest (reverse path)))
          (rest path))))))

(defn search
  [route-map from to]
  ;(print-route-map route-map)
  (dosync
    (let [[price path]
          (found-path
            (dijkstras-algorithm
              (init-graph
                route-map
                from))
            to)]
      (if (= price 0)
        {:error true}
        {:price price
         :path  (rest
                  (reduce
                    (fn [acc val]
                      (alter
                        (get (get (get (get route-map :forward) (first val)) (second val)) :tickets)
                        dec)
                      (conj acc (first val)))
                    []
                    path))}))))
