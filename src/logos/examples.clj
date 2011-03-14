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

(defn any-o [q]
  (cond-e
   (q s#)
   ((any-o q))))

(comment
  ;; hmm this might be a real bug
  (run 1 [q]
       (any-o s#)
       (== true q))

  (run* [q]
        (likes q 'mary)
        (musician q))

  ;; 2.3s for 1e6
  ;; not bad
  ;; ~1.1 for half a million
  (dotimes [_ 10]
    (time
     (dotimes [_ 5e5]
       (run* [q]
             (likes q 'mary)
             (musician q)))))
  
  ;; [john]

  (run* [q]
        (musician q))
  
  ;; [john bob]

  (run* [q]
        (exist [x y]
               (likes x y)
               (== [x y] q)))

  ;; ah yes don't forget
  ;; this will give use more answers
  ;; than we expect
  (run* [q]
        (exist [x y u v]
               (likes x y)
               (likes u v)
               (== [x y] q)))

  ;; 1.8s for 1e5
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (run* [q]
             (exist [x y]
                    (likes x y)
                    (== [x y] q))))))

  ;; this is already useful of course

  ;; not the question is how would one go about creating
  ;; a knowledge base of facts and chaining them together
  ;; in an open manner?

  ;; (?- likes 'john 'mary)
  ;; first time, creates a function likes of two arities
  ;; 2 argument, and 3 argument one that takes a next
  ;; parameter which is just a goal

  ;; (?- likes 'mary 'john)
  ;; (?- likes ?x 'mary)

  (defn likes [a b]
    (fn [x y g]
      (cond-e
       ((== x a) (== y b))
       (g))))

  (likes 'john 'mary)

  (def kb
       [(?- likes 'john 'mary)
        (?- likes 'mary 'john)])

  (defn likes
    ([x y]
       (likes x y u#))
    ([x y next]
       (cond-e
        ((== x 'john) (== y 'mary))
        ((== x 'mary) (== y 'john))
        (next))))

  (defn g1 [x g2]
    (cond-e
     ((== x 'foo))
     (g2)))

  ;; interesting we can pass goals as parameters
  (run* [q]
        (g1 q (== q 'bar)))

  (run* [q]
        (cond-e
         ()))
  )

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

(defn palindrome-m []
  (let [r (range 9)]
   (m/with-monad m/sequence-m
     (m/domonad
      [a r b r
       c r d r
       :let  [x (* (+ (* 10 a) b)
                   (+ (* 10 c) d))]
       :when (and (> x 0)
                  (>= x 100)
                  (= (mod (int (/ x 1000)) 10)
                     (mod (int (/ x 1)) 10))
                  (= (mod (int (/ x 100)) 10)
                     (mod (int (/ x 10)) 10)))]
      x))))

(comment
  (palindrome)
  (palindrome-m)
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

  ;; interesting only a hair slower than standard list comprehension
  (dotimes [_ 10]
    (time
     (dotimes [_ 1]
       (doall (palindrome-m)))))
  )