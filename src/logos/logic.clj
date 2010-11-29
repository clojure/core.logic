(ns logos.logic
  (:refer-clojure :exclude [==])
  (:use [logos.minikanren :only
         [run run* exist cond-e == lvar? rest-lvar?]]))

(defn nil-o [a]
  (== nil a))

(defn cons-o [a d l]
  (== (cons a d) l))

(defn first-o [a l]
  (exist [&d]
    (cons-o a &d l)))

(defn rest-o [l d]
  (exist [a]
    (== (cons a d) l)))

(defn pair-o [p]
  (exist [a &d]
    (== (cons a &d) p)))

(defn twin-o [p]
  )

(defn append-o [l s out]
  (cond-e
   ((nil-o l) (== s out))
   ((exist [a &d res]
           (cons-o a &d l)
           (append-o &d s res)
           (cons-o a res out)))))

(comment
  (run* [q]
        (cons-o 'a '(d) q))

  ;; Fix this, q should be nil or empty list
  (run* [&q]
        (cons-o 'a &q '(a)))

  ;; works
  (run* [q]
        (cons-o q '() '(a)))

  (run* [q]
        (exist [&d]
         (cons-o q &d [1 2])))

  (run* [q]
        (first-o q [1 2]))

  (run* [q]
        (rest-o q [1 2]))

  (run* [&q]
        (rest-o [1 2] &q))
  
  (run* [&q]
        (rest-o [1 2 3 4 5 6 7 8] &q))

  (run* [q]
        (pair-o q))

  (run* [q]
        (append-o [1 2] [3 4] q))
  )
