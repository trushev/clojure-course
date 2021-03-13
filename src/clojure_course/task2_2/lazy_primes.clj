(ns clojure-course.task2-2.lazy-primes)

(def naturals
  (lazy-seq
    (cons 2 (map inc naturals))))

(defn retract-multiples [prime numbers]
  (filter
    (fn [n] (not (= 0 (mod n prime))))
    numbers))

(defn retract [numbers]
  (retract-multiples (first numbers) (rest numbers)))

(def lazy-primes
  (map first (iterate retract naturals)))

(defn -main []
  (println (nth lazy-primes 4)))
