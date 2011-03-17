(ns logos.examples
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren)
  (:require [logos.nonrel :as nonrel]
            [clojure.contrib.monads :as m]))

(defn likes
  [x y]
  (cond-e
   ((== x 'john) (== y 'mary))
   ((== x 'mary) (== y 'john))))

(defn musician [x]
  (cond-e
   ((== x 'john))
   ((== x 'bob))))

;; from CTM

(defn digit-o [x]
  (cond-e
   ((== 0 x))
   ((== 1 x))
   ((== 2 x))
   ((== 3 x))
   ((== 4 x))
   ((== 5 x))
   ((== 6 x))
   ((== 7 x))
   ((== 8 x))
   ((== 9 x))))

(comment
  ;; of course this is better
  (defrel digit #{0 1 2 3 4 5 6 7 8 9})
  )

;; it is nice that in Mozart you don't need to use project
;; for the following

(defn palindrome-o [x]
  (exist [a b c d]
    (digit-o a) (digit-o b) (digit-o c) (digit-o d)
    (nonrel/project [a b c d]
     (== x (* (+ (* 10 a) b)
              (+ (* 10 c) d)))
     (nonrel/project [x]
      (== (> x 0) true)
      (== (>= x 100) true)
      (== (mod (int (/ x 1000)) 10)
          (mod (int (/ x 1)) 10))
      (== (mod (int (/ x 100)) 10)
          (mod (int (/ x 10)) 10))))))

(defn palindrome []
  (let [r (range 9)]
    (for [a r b r
          c r d r
          :let  [x (* (+ (* 10 a) b)
                      (+ (* 10 c) d))]
          :when (and (> x 0)
                     (>= x 100)
                     (= (mod (int (/ x 1000)) 10)
                        (mod (int (/ x 1)) 10))
                     (= (mod (int (/ x 100)) 10)
                        (mod (int (/ x 10)) 10)))]
      x)))


(comment
  (palindrome)
  (run* [q] (palindrome-o q))

  ;; 40ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1]
       (run* [q] (palindrome-o q)))))

  ;; ~4ms, so about 10X faster, not as dramatic a difference
  ;; as I would have imagined!
  (dotimes [_ 10]
    (time
     (dotimes [_ 1]
       (doall (palindrome)))))
  )