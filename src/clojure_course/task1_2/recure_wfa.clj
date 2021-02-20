(ns clojure-course.task1-2.recure-wfa)

(defn alphabet-loop
  ([word alphabet] (alphabet-loop word alphabet (list)))
  ([word alphabet result]
   (if (empty? alphabet)
     result
     (let [char   (first alphabet)
           result (if (.endsWith word char)
                    result
                    (cons (str word char) result))]
       (recur word (rest alphabet) result)))))

(defn words-loop
  ([words alphabet] (words-loop words alphabet (list)))
  ([words alphabet result]
   (if (empty? words)
     result
     (recur
       (rest words)
       alphabet
       (concat (alphabet-loop (first words) alphabet) result)))))

(defn word-length-loop
  ([alphabet length] (word-length-loop alphabet (dec length) alphabet))
  ([alphabet length result]
   (if (= length 0)
     result
     (recur
       alphabet
       (dec length)
       (words-loop result alphabet)))))

(defn wfa-by-tail-recur
  [alphabet word-length]
  {:pre [(>= word-length 0)]}
  (if (= word-length 0)
    (list)
    (word-length-loop alphabet word-length)))
