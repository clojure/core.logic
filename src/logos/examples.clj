(ns logos.examples
  (:refer-clojure :exclude [reify ==])
  (:use logos.minikanren)
  (:require [logos.nonrel :as nonrel]))

(defn likes
  [x y]
  (cond-e
   ((== x 'john) (== y 'mary))
   ((== x 'mary) (== y 'john))))

(defn musician [x]
  (cond-e
   ((== x 'john))
   ((== x 'bob))))

(comment

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

(defn digit [x]
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

(defn alpha [x]
  (cond-e
   ((== 'a x))
   ((== 'b x))
   ((== 'c x))
   ((== 'd x))
   ((== 'e x))
   ((== 'f x))
   ((== 'g x))
   ((== 'h x))
   ((== 'i x))
   ((== 'j x))
   ((== 'k x))
   #_((== 'l x))))

(comment
  ;; of course this is better
  (defrel digit #{0 1 2 3 4 5 6 7 8 9})
  )

;; it is nice that in Mozart you don't need to use project
;; for the following

(defn palindrome [x]
  (exist [a b c d]
    (digit a) (digit b) (digit c) (digit d)
    (nonrel/project [a b c d]
     (== x (* (+ (* 10 a) b)
              (+ (* 10 c) d)))
     (nonrel/project [x]
      (== (> x 0) true)
      (== (>= x 100) true)
      (== (mod (/ x 1000) 10) (mod (/ x 1) 10))
      (== (mod (/ x 100)) (mod (/ x 10) 10))))))

(comment

  (let [r (run* [q]
            (exist [a b c d]
               (digit a) (digit b) (digit c) (digit d)
               (nonrel/project [a b c d]
                  (== q (* (+ (* 10 a) b)
                           (+ (* 10 c) d))))))]
    (count r))


  ;; NOTE: this ordering works
  (run* [q]
        (exist [a b]
               (== q a)
               (digit a)
               (digit b)))

  ;; works
  (run* [q]
        (exist [a b]
               (digit a)
               (== q a)
               (digit b)))

  ;; FIXME: this ordering does not
  ;; there is a bug related to unifying with zero - David
  (run* [q]
        (exist [a b]
               (digit a)
               (digit b)
               (== q a)))

  ;; SMOKING GUN
  ;; our mplus implementation is suspect, we have an EVEN/ODD problem
  ;; if two cond-e expressions follow one another and there are an even
  ;; number of cond-e clauses - it fails, (with trying to  walk nil error)
  ;; if the number of cond-e clauses is odd, everything works fine
  (run* [q]
        (exist [a b c d]
               (alpha a)
               (alpha b)
               (== q a)))

  ;; ugh
  ;; fail for some reason
  (run* [q]
        (exist [a b]
               (digit a)
               (digit b)
               (== q [a])))

  ;; works
  (run* [q]
        (exist [a b]
               (digit a)
               (== q [a])))
  
  ;; (5)
  ;; as expected
  (run* [q]
    (exist [a b c]
     (== 1 a)
     (== 2 b)
     (== 3 c)
     (nonrel/project [a b c]
       (== q (+ (* a b) c)))))

  )

(comment
  (defmulti multi-test type)
  (defmethod multi-test ::a [x])
  (defmethod multi-test ::b [x])

  ;; multimethods are almost twice as fast
  ;; can we get very close to that?
  ;; perhaps via tabling
  (dotimes [_ 10]
    (let [x (with-meta {} {:type ::a})]
     (time
      (dotimes [_ 8e6]
        (multi-test x)))))
  )
>