(ns clojure-course.task1-4.core)

(defn alphabet-loop
  ([word alphabet]
   (reduce
    (fn [words char]
      (cons (str word char) words))
    (list)
    (filter
     (fn [char] (not (.endsWith word char)))
     alphabet))))

(defn words-loop
  ([words alphabet]
   (reduce
    (fn [words-acc word]
      (concat words-acc (alphabet-loop word alphabet)))
    (list)
    words)))

(defn word-length-loop
  ([alphabet length]
   (reduce
    (fn [words _] (words-loop words alphabet))
    alphabet
    (range 1 length))))

(defn wfa-by-reduce
  [alphabet word-length]
  {:pre [(>= word-length 0)]}
  (if (= word-length 0)
    (list)
    (word-length-loop alphabet word-length)))
