(ns clojure.core.logic.prelude
  (:refer-clojure :exclude [reify == inc])
  (:use clojure.core.logic.minikanren)
  (:require [clojure.set :as set]
            [clojure.core.logic.nonrel :as nonrel]
            clojure.core.logic.tabled
            [clojure.core.logic.match :as match]))

;; =============================================================================
;; Basics from The Reasoned Schemer

(defn nilo [a]
  (== nil a))

(defn emptyo [a]
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
    ((emptyo l) (== s out))
    ((exist [a d res]
       (conso a d l)
       (conso a res out)
       (appendo d s res)))))

(defn flatteno [s out]
  (conde
    ((emptyo s) (== '() out))
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
  (apply match/defnm 'clojure.core.logic.nonrel/conda rest))

(defmacro defnu [& rest]
  (apply match/defnm 'clojure.core.lgoic.nonrel/condu rest))

(defmacro matcha [xs & cs]
  (match/handle-clauses 'clojure.core.logic.nonrel/conda xs cs))

(defmacro matchu [xs & cs]
  (match/handle-clauses 'clojure.core.logic.nonrel/condu xs cs))

;; =============================================================================
;; Facts

(defn index [tuples]
  (->> tuples
       (map (fn [[k :as t]] {k #{t}}))
       (apply merge-with
              (fn [a b] (set/union a b)))))

(defmacro defrel [name & args]
  (let [setsym (symbol (str name "-set"))
        idxsym (symbol (str name "-indexed"))]
    `(do
       (def ~setsym (atom #{}))
       (def ~idxsym (atom {}))
       (defmacro ~name [~@args]
         (defrelg '~setsym '~idxsym ~@args)))))

(defn defrelg [setsym idxsym & args]
  `(fn [a#]
     (answers a# (deref ~setsym) (deref ~idxsym) [~@args])))

;; NOTE: put in a dosync?

(defmacro fact [rel & tuple]
  (let [setsym (symbol (str rel "-set"))
        idxsym (symbol (str rel "-indexed"))]
    `(do
       (swap! ~setsym conj [~@tuple])
       (reset! ~idxsym (index @~setsym))
       nil)))

(defn to-stream [aseq]
  (when (seq aseq)
    (choice (first aseq)
            (fn [] (to-stream (next aseq))))))

(defn answers [a aset indexed [f & r :as t]]
  (let [v (walk a f)
        aset (if (lvar? v)
               aset
               (indexed v))]
    (to-stream
     (->> aset
          (map (fn [cand]
                 (when-let [a (unify a t cand)]
                   a)))
          (remove nil?)))))