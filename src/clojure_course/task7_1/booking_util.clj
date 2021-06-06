(ns clojure-course.task7-1.booking_util)

;
; A ----------> B <------------> C
;    10/1          20/2    35/3
;
; route-map:
; {
; 	forward: {
; 		A: {B: {price: 10, tickets: 1}},
; 		B: {C: {price: 20, tickets: 2}},
; 		C: {B: {price: 35, tickets: 3}}
; 	},
; 	backward: {
; 		B: {A: {price: 10, tickets: 1}, C: {price: 35, tickets: 3}},
; 		C: {B: {price: 20, tickets: 2}}
; 	}
; }
;
; dijkstra-algorithm:
; {
;   not-visited: {},
;   visited: {A: 0, B: 10, C: 30},
;   paths: {A [A], B [A B], C [A B C]}
;   edges: {
;     A: {B: 10},
;     B: {C: 20},
;     C: {B: 35}
;   }
; }
;

(defn- sort-map
  [a-map]
  (sort-by last a-map))

(defn- not-visited
  [graph]
  (first (first (sort-map (graph :not-visited)))))

(defn- neighbors
  [graph vertex]
  (map first (sort-map ((graph :edges) vertex))))

(defn- vertex-status
  [graph vertex]
  (if (nil? ((graph :not-visited) vertex))
    :visited
    :not-visited))

(defn- updated-graph
  [graph outer-key inner-key value]
  (assoc
    graph
    outer-key
    (assoc (graph outer-key) inner-key value)))

(defn- updated-marker
  [graph status vertex marker]
  (updated-graph graph status vertex marker))

(defn- updated-path
  [graph source dest]
  (updated-graph graph :paths dest (conj ((graph :paths) source) dest)))

(defn- marked
  [graph source dest]
  (let [source-mark ((graph :not-visited) source)
        dest-status (vertex-status graph dest)
        dest-mark ((graph dest-status) dest)
        edge (((graph :edges) source) dest)
        new-mark (+ source-mark edge)
        need-update? (> dest-mark new-mark)]
    (if need-update?
      (updated-path
        (updated-marker graph dest-status dest new-mark)
        source
        dest)
      graph)))

(defn- removed-not-visited
  [graph vertex]
  (assoc
    graph
    :not-visited
    (dissoc (graph :not-visited) vertex)))

(defn- visit
  [graph vertex]
  (updated-graph
    (removed-not-visited graph vertex)
    :visited
    vertex
    ((graph :not-visited) vertex)))

(defn- dijkstra-algorithm
  [graph]
  (let [not-visited (not-visited graph)]
    (if (nil? not-visited)
      graph
      (-> (reduce
            (fn [graph neighbor] (marked graph not-visited neighbor))
            graph
            (neighbors graph not-visited))
          (visit not-visited)
          (recur)))))

(defn- init-edges
  [route-map edges]
  (reduce
    (fn [edges [source destinations]]
      (assoc
        edges
        source
        (->> destinations
             (map (fn [[v pt]] (list v (pt :price) @(pt :tickets))))
             (filter (fn [[_ _ t]] (> t 0)))
             (map (fn [[v p _]] (list v p)))
             (reduce (fn [acc [v p]] (assoc acc v p)) {}))))
    edges
    (route-map :forward)))

(defn- init-map
  [keys value]
  (reduce
    (fn [map key] (assoc map key value))
    {}
    keys))

(defn- init-graph
  [route-map start-vertex]
  (let [vertexes (set (concat (keys (route-map :forward)) (keys (route-map :backward))))]
    (-> {}
        (assoc :not-visited (assoc (init-map vertexes ##Inf) start-vertex 0))
        (assoc :visited {})
        (assoc :paths {start-vertex [start-vertex]})
        (assoc :edges (init-edges route-map (init-map vertexes {}))))))

(defn- found-path
  [graph end]
  (if (= ((graph :visited) end) ##Inf)
    [0 []]
    [((graph :visited) end)
     (let [path ((graph :paths) end)]
       (map
         (fn [x y] [x y])
         (reverse (rest (reverse path)))
         (rest path)))]))

(defn- book-ticket!
  [route-map from to]
  (alter ((((route-map :forward) from) to) :tickets) dec)
  to)

(defn search
  [route-map from to]
  (dosync
    (let [[price path]
          (-> route-map
              (init-graph from)
              (dijkstra-algorithm)
              (found-path to))]
      [price (reduce
               (fn [cities [from to]]
                 (conj cities (book-ticket! route-map from to)))
               [from]
               path)])))
