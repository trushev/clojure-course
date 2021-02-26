(ns clojure-course.task2-1.sieve)

(defn prime? [n primes]
  (let [[prime? new-primes] (reduce
                             (fn [acc val]
                               (let [[prime? new-primes] acc
                                     [prime comb]        val]
                                 (if (= n comb)
                                   (list false (conj new-primes (list prime (+ prime comb))))
                                   (list prime? (conj new-primes val)))))
                             (list true [])
                             primes)]
    (list prime?
          (if prime?
            (conj new-primes (list n (* n n)))
            new-primes))))

(defn prime [nth]
  (loop [i 0]))
