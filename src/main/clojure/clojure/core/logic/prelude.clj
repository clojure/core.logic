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

(defn defrel-helper [name arity args]
  (let [r (range 1 (+ arity 2))
        arity-excs (fn [n] `(arity-exc-helper '~name ~n))]
    (if (seq args)
      `(do
         (def ~name (~'Rel. '~name (atom {}) nil ~@(map arity-excs r)))
         (extend-rel ~name ~@args))
      `(def ~name (~'Rel. '~name (atom {}) nil ~@(map arity-excs r))))))

(defprotocol IRel
  (setfn [this arity f])
  (indexes-for [this arity])
  (add-indexes [this arity index]))

;; TODO: consider moving the set/indexes inside Rel, perf implications?

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
         (~'indexes-for [~'_ ~'arity]
           ((deref ~'indexes) ~'arity))
         (~'add-indexes [~'_ ~'arity ~'index]
           (swap! ~'indexes assoc ~'arity ~'index)))
       (defmacro ~'defrel [~'name ~'& ~'rest]
         (defrel-helper ~'name ~arity ~'rest)))))

(defn index-sym [name arity o]
  (->sym name "_" arity "-" o "-index"))

(defn set-sym [name arity]
  (->sym name "_" arity "-set"))

;; TODO: for arity greater than 20, we need to use rest args
;; TODO: applyToHelper
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
                       `((not (~'lvar? (~'walk ~'a ~a))) ((deref ~(index-sym name arity o)) ~a))))
        indexed-set (fn [[o i]]
                      `(def ~(index-sym name arity o) (atom {})))]
    (if (<= arity 20)
     `(do
        (def ~(set-sym name arity) (atom #{}))
        ~@(map indexed-set indexed)
        (add-indexes ~name ~arity '~indexed)
        (setfn ~name ~arity
               (fn [~@as]
                 (fn [~'a]
                   (let [set# (cond
                               ~@(mapcat check-lvar indexed)
                               :else (deref ~(set-sym name arity)))]
                     (~'to-stream
                      (->> set#
                           (map (fn [cand#]
                                  (when-let [~'a (~'unify ~'a [~@as] cand#)]
                                    ~'a)))
                           (remove nil?)))))))))))

;; TODO: Should probably happen in a transaction

(defn facts
  ([rel [f :as tuples]] (facts rel (count f) tuples))
  ([^Rel rel arity tuples]
     (let [rel-set (var-get (resolve (set-sym (.name rel) arity)))
           tuples (map vec tuples)]
       (swap! rel-set (fn [s] (into s tuples)))
       (let [indexes (indexes-for rel arity)]
         (doseq [[o i] indexes]
           (let [index (var-get (resolve (index-sym (.name rel) arity o)))]
             (let [indexed-tuples (map (fn [t]
                                         {(nth t (dec i)) #{t}})
                                       tuples)]
               (swap! index
                      (fn [i]
                        (apply merge-with set/union i indexed-tuples))))))))))

(defn fact [rel & tuple]
  (facts rel [(vec tuple)]))

;; work to do
(comment
  ;; BUG: deftype mutable field are not visible and printing type causes confusing error

  (RelHelper 20)
  (defrel foo)
  (foo 1 2)

  (defrel friends ^:index person1 ^:index person2)
  
  (facts friends
         '[[John Jill]
           [Tom Jill]
           [Jill Mary]
           [Thomas Mary]
           [Ben Carey]
           [Lisa Tom]
           [George Henry]
           [Henry Wendy]
           [Henry Peter]
           [Henry David]
           [Willis Tracy]
           [Hal Tracy]
           [Cathy Karl]
           [Tony Tilly]
           [Greg Rob]
           [Ulrika Evan]
           [Dan Ohal]
           [Trevor Spencer]
           [Bob Bill]])

  ;; 1000 friends
  (facts friends
         (conj (into [] (map (juxt ->sym ->sym) (range 1 1e3)))
               '[Bob Bill]))

  (run* [q]
    (friends q 'Bill))

  ;; 400ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (run* [q]
         (friends 'Bob q)))))

  (defn friends-slow [x y]
    (conde
      ((== '[John Jill] [x y]))
      ((== '[Tom Jill] [x y]))
      ((== '[Jill Mary] [x y]))
      ((== '[Thomas Mary] [x y]))
      ((== '[Ben Carey] [x y]))
      ((== '[Lisa Tom] [x y]))
      ((== '[George Henry] [x y]))
      ((== '[Henry Wendy] [x y]))
      ((== '[Henry Peter] [x y]))
      ((== '[Henry David] [x y]))
      ((== '[Willis Tracy] [x y]))
      ((== '[Hal Tracy] [x y]))
      ((== '[Cathy Karl] [x y]))
      ((== '[Tony Tilly] [x y]))
      ((== '[Greg Rob] [x y]))
      ((== '[Ulrika Evan] [x y]))
      ((== '[Dan Ohal] [x y]))
      ((== '[Trevor Spencer] [x y]))
      ((== '[Bob Bill] [x y]))))

  (defn friends-one [x y]
    (conde
      ((== '[Bob Bill] [x y]))))

  (run* [q]
    (friends-slow 'Bob q))

  ;; 200ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (run* [q]
         (friends-one 'Bob q)))))

  ;; 1.3s, grows with size of conde
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (run* [q]
         (friends-slow 'Bob q)))))

  ;; compound keys syntax
  (defrel person)
  (extend-rel ^{:index true :name "name"} first-name
              ^{:index true :name "name"} last-name)
  )