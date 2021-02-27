(ns clojure-course.task2-1.sieve)

(defn test-prime [n primes]
  (let [[prime?
         new-primes] (reduce
                      (fn [acc val]
                        (let [[prime? new-primes] acc
                              [prime comb]        val]
                          (if (= n comb)
                            [false (conj new-primes [prime (+ prime comb)])]
                            [prime? (conj new-primes val)])))
                      [true []]
                      primes)]
    (if prime?
      (conj new-primes [n (* n n)])
      new-primes)))

(defn prime [nth]
  (loop [cnt       2
         primes    []]
    (let [new-primes (test-prime cnt primes)]
      (if (= (count new-primes) nth)
        (first (peek new-primes))
        (recur (inc cnt) new-primes)))))
