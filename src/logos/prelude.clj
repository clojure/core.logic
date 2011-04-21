(ns logos.prelude
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren)
  (:require logos.tabled
            [logos.match :as match]))


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

;; =============================================================================
;; Convenient Goal Fns

(defn defn-m [t n as & cs]
  (if-let [tabled? (-> n meta :tabled)]
    `(def ~n
       (logos.tabled/tabled [~@as]
        ~(match/handle-clauses t as cs)))
    `(defn ~n [~@as]
       ~(match/handle-clauses t as cs))))

(defmacro defn-e [& rest]
  (apply defn-m `cond-e rest))

(defmacro match-e [xs & cs]
  (match/handle-clauses `cond-e xs cs))

;; -----------------------------------------------------------------------------
;; defn-u, defn-a, match-a, match-u

(defmacro defn-a [& rest]
  (apply defn-m `cond-a rest))

(defmacro defn-u [& rest]
  (apply defn-m `cond-u rest))

(defmacro match-a [xs & cs]
  (match/handle-clauses `cond-a xs cs))

(defmacro match-u [xs & cs]
  (match/handle-clauses `cond-u xs cs))
