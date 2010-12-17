(ns logos.scratch
  (:refer-clojure :exclude [reify ==]))

(defn sum [acc n]
  (if (zero? n)
    acc
    (recur (inc acc) (dec n))))

(sum 0 1000000)

(comment
  )