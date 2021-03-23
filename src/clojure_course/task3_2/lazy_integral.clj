(ns clojure-course.task3-2.lazy-integral)

(defn square [a b h]
  (* 0.5 h (+ a b)))

(defn f-square [f k h]
  (square
    (f (* k h))
    (f (* (inc k) h))
    h))

(defn point-count [x h]
  (int (/ x h)))

(defn last-point [x h]
  (* h (point-count x h)))

(defn integral-rest [f x h]
  (let [last-point (last-point x h)]
    (square
      (f last-point)
      (f x)
      (- x last-point))))

(defn square-seq [f h]
  (iterate
    (fn [[square k]]
      (list
        (+ square (f-square f (inc k) h))
        (inc k)))
    (list (f-square f 0 h) 0)))

(defn integral [f x h ss]
  (+
    (integral-rest f x h)
    (first
      (nth
        ss
        (dec (point-count x h))))))

(defn operator [f h]
  (let [ss (square-seq f h)]
    (fn [x]
      (if (<= x 0)
        0
        (integral f x h ss)))))
