(ns logos.logic
  (:require [mi]))

(defn nil-o [a]
  (== nil a))

(defn cons-o [a l]
  (exist [c &d]
         (== (cons a &d) l)))

(defn rest-o [l d]
  (exist [a]
         (== (cons a d) l)))

(defn first-o [a l]
  (cons-o a l))

(defn pair-o [p]
  ())

(defn twin-o [p]
  )

(defn append-o [a b]
  )
