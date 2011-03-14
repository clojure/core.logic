(ns logos.logic
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren))

(defn null-o [a]
  (== '() a))

(defn cons-o [a d l]
  (== (lcons a d) l))

(defn first-o [l a]
  (exist [d]
    (cons-o a d l)))

(defn rest-o [l d]
  (exist [a]
    (== (lcons a d) l)))

(defn pair-o [p]
  (exist [a d]
    (== (lcons a d) p)))

(defn twin-o [p]
  (exist [x]
         (cons-o x x p)))

(defn append-o [l s out]
  (cond-e
   ((null-o l) (== s out))
   ((exist [a d res]
           (cons-o a d l)
           (cons-o a res out)
           (append-o d s res)))))

(defn flatten-o [s out]
  (cond-e
   ((null-o s) (== '() out))
   ((pair-o s)
    (exist [a d res-a res-d]
           (cons-o a d s)
           (flatten-o a res-a)
           (flatten-o d res-d)
           (append-o res-a res-d out)))
   ((cons-o s '() out))))

(defn member-o [x l]
  (cond-e
   ((first-o l x))
   ((exist [r]
           (rest-o l r)
           (member-o x r)))))

(defn rember-o [x l out]
  (cond-e
   ((== '() l) (== '() out))
   ((exist [a d]
      (cons-o a d l)
      (== x a)
      (== d out)))
   ((exist [a d res]
      (cons-o a d l)
      (cons-o a res out)
      (rember-o x d res)))))