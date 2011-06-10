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

;; we want a macro to do this crap for us
;; we need to be able to set the fns
(deftype Rel [name
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f1
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f2
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f3
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f4
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f5
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f6
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f7
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f8
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f9
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f10
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f11
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f12
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f13
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f14
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f15
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f16
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f17
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f18
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f19
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f20
              ^{:unsynchronized-mutable true :tag clojure.lang.IFn} f21]
  clojure.lang.IFn
  (invoke [_ a1]
    (f1 a1))
  (invoke [_ a1 a2]
    (f2 a1 a2))
  (invoke [_ a1 a2 a3]
    (f3 a1 a2 a3))
  (invoke [_ a1 a2 a3 a4]
    (f4 a1 a2 a3 a4))
  (invoke [_ a1 a2 a3 a4 a5]
    (f5 a1 a2 a3 a4 a5))
  (invoke [_ a1 a2 a3 a4 a5 a6]
    (f6 a1 a2 a3 a4 a5 a6))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7]
    (f7 a1 a2 a3 a4 a5 a6 a7))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8]
    (f8 a1 a2 a3 a4 a5 a6 a7 a8))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9]
    (f9 a1 a2 a3 a4 a5 a6 a7 a8 a9))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10]
    (f10 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11]
    (f11 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12]
    (f12 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13]
    (f13 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14]
    (f14 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15]
    (f15 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16]
    (f16 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17]
    (f17 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18]
    (f18 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19]
    (f19 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20]
    (f20 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 rest]
    (f21 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 rest))
  (applyTo [_ arglist]
    ))

(def foo (Rel. 'foo (fn [a1] :foo)
               nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
               (fn [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 & a21]
                 :bar)))

(defmacro RelHelper [arity]
  (let [r (range 1 arity)
        sym-helper (fn [prefix]
                     (fn [n] (symbol (str prefix n))))
        f-sym (sym-helper "f")
        a-sym (sym-helper "a")
        fs (map #(with-meta % {:mutable-volatile true :tag clojure.lang.IFn})
                (map f-sym r))
        create-sig (fn [n]
                     (let [args (map a-sym (range 1 (inc n)))]
                      `(~'invoke [_ ~@args]
                                 (~(f-sym n) ~@args))))]
   `(deftype ~'Rel [~'name ~'meta
                  ~@fs]
      clojure.lang.IFn
      ~@(map create-sig r))))

(RelHelper 20)

;; work to do
(comment
  ;; 400ms, plenty fast
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e8]
       (foo 1))))

  ;; TODO: fix
  (apply foo [1])

  ;; voila
  (foo 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23)
  ;; complex indexes?
  )