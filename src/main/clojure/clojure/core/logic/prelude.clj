(ns clojure.core.logic.prelude
  (:refer-clojure :exclude [reify == inc])
  (:use clojure.core.logic.minikanren)
  (:require clojure.core.logic.tabled
            [clojure.core.logic.match :as match]))


(defn nullo [a]
  (== '() a))

(defn conso [a d l]
  (== (lcons a d) l))

(defn firsto [l a]
  (exist [d]
    (conso a d l)))

(defn resto [l d]
  (exist [a]
    (== (lcons a d) l)))

(defn pairo [p]
  (exist [a d]
    (== (lcons a d) p)))

(defn twino [p]
  (exist [x]
    (conso x x p)))

(defn appendo [l s out]
  (conde
    ((nullo l) (== s out))
    ((exist [a d res]
       (conso a d l)
       (conso a res out)
       (appendo d s res)))))

(defn flatteno [s out]
  (conde
    ((nullo s) (== '() out))
    ((pairo s)
     (exist [a d res-a res-d]
       (conso a d s)
       (flatteno a res-a)
       (flatteno d res-d)
       (appendo res-a res-d out)))
    ((conso s '() out))))

(defn membero [x l]
  (conde
    ((firsto l x))
    ((exist [r]
       (resto l r)
       (membero x r)))))

(defn rembero [x l out]
  (conde
    ((== '() l) (== '() out))
    ((exist [a d]
       (conso a d l)
       (== x a)
       (== d out)))
    ((exist [a d res]
       (conso a d l)
       (conso a res out)
       (rembero x d res)))))

;; =============================================================================
;; Convenient Goal Fns

(defmacro defne [& rest]
  (apply match/defnm `conde rest))

(defmacro matche [xs & cs]
  (match/handle-clauses `conde xs cs))

;; -----------------------------------------------------------------------------
;; defnu, defna, matcha, matchu

(defmacro defna [& rest]
  (apply match/defnm `conda rest))

(defmacro defnu [& rest]
  (apply match/defnm `condu rest))

(defmacro matcha [xs & cs]
  (match/handle-clauses `conda xs cs))

(defmacro matchu [xs & cs]
  (match/handle-clauses `condu xs cs))
