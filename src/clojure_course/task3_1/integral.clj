(ns clojure-course.task3-1.integral)

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

(def integral-mem
  (memoize
   (fn [f k h]
     (let [f-square (f-square f k h)]
       (if (= k 0)
         f-square
         (+ f-square (integral-mem f (dec k) h)))))))

(defn integral [f x h]
  (+
   (integral-mem f (dec (point-count x h)) h)
   (integral-rest f x h)))

(defn operator [f h]
  (fn [x]
    (if (<= x 0)
      0
      (integral f x h))))
