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

;; -----------------------------------------------------------------------------
;; Rel

(defn arity-exc-helper [name n]
  (fn [& args]
    (throw (clojure.lang.ArityException. n (str name)))))

(defn sym-helper [prefix n]
  (symbol (str prefix n)))

(def f-sym (partial sym-helper "f"))
(def a-sym (partial sym-helper "a"))

(defn ->sym [& args]
  (symbol (apply str args)))

(defn defrel-helper [name arity]
  (let [r (range 1 (+ arity 2))
        arity-excs (fn [n] `(arity-exc-helper '~name ~n))]
   `(def ~name (~'Rel. '~name (atom {}) nil ~@(map arity-excs r)))))

(defprotocol IRel
  (setfn [this arity f])
  (index-for [this arity])
  (add-index [this arity index]))

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
       (deftype ~'Rel [~'name ~'indexes ~'meta
                       ~@mfs]
         clojure.lang.IObj
         (~'withMeta [~'_ ~'meta]
           (~'Rel. ~'name ~'indexes ~'meta ~@fs))
         (~'meta [~'_]
           ~'meta)
         clojure.lang.IFn
         ~@(map create-sig r)
         ~'IRel
         (~'setfn [~'_ ~'arity ~'f]
           (case ~'arity
                 ~@(mapcat set-case (map vector fs r))))
         (~'index-for [~'_ ~'arity]
           ((deref ~'indexes) ~'arity))
         (~'add-index [~'_ ~'arity ~'index]
           (swap! ~'indexes assoc ~'arity ~'index)))
       (defmacro ~'defrel [~'name]
         (defrel-helper ~'name ~arity)))))

;; 3) for arity greater than 20, we need to use rest args
;; 4) fact will need to understand arities as well
;; 5) applyToHelper
(defmacro extend-rel [name & args]
  (let [arity (count args)
        r (range 1 (clojure.core/inc arity))
        as (map a-sym r)
        indexed (filter (fn [[a i]]
                          (-> a meta :index))
                        (map vector
                             args
                             (range 1 (clojure.core/inc arity))))
        check-lvar (fn [[o i]]
                     (let [a (a-sym i)]
                       `((not (~'lvar? (~'walk ~'a ~a))) (~(->sym name "_" arity "-" o "-index") ~a))))
        indexed-set (fn [[o i]]
                      `(def ~(->sym name "_" arity "-" o "-index") (atom #{})))]
    (println indexed)
    (if (<= arity 20)
     `(do
        (def ~(->sym name "_" arity "-set") (atom #{}))
        ~@(map indexed-set indexed)
        (setfn ~name ~arity
               (fn [~@as]
                 (fn [~'a]
                   (let [set# (cond
                               ~@(mapcat check-lvar indexed)
                               :else ~(->sym name "_" arity "-set"))]
                     (~'to-stream
                      (->> set#
                           (map (fn [cand#]
                                  (when-let [~'a (~'unify ~'a [~@as] cand#)]
                                    ~'a)))
                           (remove nil?)))))))))))

;; deffact

;; work to do
(comment
  ;; BUG: mutable field are not visible and printing type causes confusing error
  (RelHelper 20)
  (defrel foo)
  (foo 1 2)

  ;; extend-rel should be a macro
  ;; taking a rel name and args with indexing directives
  ;; this will generate an appropiate fn for that rel
  ;; there is no expectation for extend to be a fn
  ;; most specific to least specific key
  ;; design is slow
  ;; each index is by arity and which part is being indexed

  (defrel friends)
  (extend-rel friends ^:index person1 ^:index person2)
  (extend-rel friends person1 person2 person3)

  (defrel person)
  (extend-rel ^{:index true :name "name"} first-name
              ^{:index true :name "name"} last-name)

  (doseq [f [[person "Bob" "Smith"]
             [person "John" "Smith"]
             [person "Ray" "Smith"]]]
    (apply fact f))

  ;; wondering about positional nature
  ;; we could support take a map and normalizing to the
  ;; positioned args
  )