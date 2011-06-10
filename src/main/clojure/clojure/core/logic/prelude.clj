(ns clojure.core.logic.prelude
  (:refer-clojure :exclude [reify == inc])
  (:use clojure.core.logic.minikanren)
  (:require [clojure.set :as set]
            [clojure.core.logic.nonrel :as nonrel]
            [clojure.core.logic.tabled :as tabled]
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

(defn listo [l]
  (conde
    ((emptyo l) s#)
    ((pairo l)
     (exist [d]
       (resto l d)
       (listo d)))))

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
  (apply match/defnm 'clojure.core.logic.nonrel/condu rest))

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

(defn arity-exc-helper [name n]
  (fn [& args]
    (throw (clojure.lang.ArityException. n (str name)))))

(defn defrel-helper [name arity]
  (let [r (range 1 (+ arity 2))
        arity-excs (fn [n] `(arity-exc-helper '~name ~n))]
   `(def ~name (~'Rel. '~name nil ~@(map arity-excs r)))))

(defn sym-helper [prefix n]
  (symbol (str prefix n)))

(def f-sym (partial sym-helper "f"))
(def a-sym (partial sym-helper "a"))

(defmacro RelHelper [arity]
  (let [r (range 1 (+ arity 2))
        fs (map f-sym r)
        mfs (map #(with-meta % {:volatile-mutable true :tag clojure.lang.IFn})
                 fs)
        create-sig (fn [n]
                     (let [args (map a-sym (range 1 (clojure.core/inc n)))]
                       `(~'invoke [~'_ ~@args]
                                  (~(f-sym n) ~@args))))
        set-case (fn [[f arity]]
                   `(~arity (set! ~f ~'f)))]
    `(do
       (defprotocol ~'IRel
         (~'setfn [~'this ~'arity ~'f]))
       (deftype ~'Rel [~'name ~'meta
                       ~@mfs]
         clojure.lang.IObj
         (~'withMeta [~'_ ~'meta]
           (~'Rel. ~'name ~'meta ~@fs))
         (~'meta [~'_]
           ~'meta)
         clojure.lang.IFn
         ~@(map create-sig r)
         ~'IRel
         (~'setfn [~'_ ~'arity ~'f]
           (case ~'arity
                 ~@(mapcat set-case (map vector fs r)))))
       (defmacro ~'defrel [~'name]
         (defrel-helper ~'name ~arity)))))

;; figure out which args are indexed
;; need to store the index for a arity somewhere
;; for arity greater than 20, we need to use rest args
;; fact will need to understand arities as well
;; applyToHelper
(defmacro extend-rel [name & args]
  (let [arity (count args)
        r (range 1 (clojure.core/inc arity))]
   `(setfn ~name ~arity
           (fn [~(map a-sym r)]
             ))))

;; deffact

;; work to do
(comment
  ;; BUG: mutable field are not visible and printing type causes confusing error
  (RelHelper 18)
  (defrel foo)

  (foo 1 2)
  
  (fn [a b c d e f g h i j k l m n o p q r s t & u])

  ;; extend-rel should be a macro
  ;; taking a rel name and args with indexing directives
  ;; this will generate an appropiate fn for that rel
  ;; there is no expectation for extend to be a fn

  (defrel friends)
  (extend-rel ^:index person1 ^:index person2)

  (defrel person)
  (extend-rel ^{:index true :name "name"} first-name
              ^{:index true :name "name"} last-name)
  )