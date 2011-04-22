(ns logos.match
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren)
  (:require [clojure.set :as set])
  (:import [logos.minikanren Substitutions]))

(declare p->term)

(defn lcons-p? [p]
  (and (coll? p)
       (not (nil? (some '#{.} p)))))

(defn p->llist [p]
  `(llist
    ~@(map p->term
           (remove #(contains? '#{.} %) p))))

(defn p->term [p]
  (cond
   (= p '_) `(lvar)
   (lcons-p? p) (p->llist p)
   (and (coll? p)
        (not= (first p) 'quote)) `[~@(map p->term p)]
        :else p))

(defn lvar-sym? [s]
  (= (first (str s)) \?))

(defn extract-vars
  ([p]
     (set
      (cond
       (lvar-sym? p) [p]
       (coll? p) (filter lvar-sym? (flatten p))
       :else nil)))
  ([p seen]
     (set/difference (extract-vars p) (set seen))))

(defn exist? [cs]
  (= (first cs) `exist))

(defn ex
  ([vs t a]
     `(exist [~@vs]
        (== ~t ~a)))
  ([vs t a exprs]
     (if (exist? exprs)
       `(exist [~@vs]
          (== ~t ~a)
          ~exprs)
       `(exist [~@vs]
          (== ~t ~a)
          ~@exprs))))

(defn ex* [[[p a :as pa] & par] exprs seen]
  (let [t (p->term p)
        vs (extract-vars p seen)
        seen (reduce conj seen vs)]
    (cond
     (nil? pa) exprs
     (= p '_) (ex* par exprs seen)
     (empty? par) (if exprs
                    (ex vs t a exprs)
                    (ex vs t a))
     :else (let [r (ex* par exprs seen)]
             (if r
               (ex vs t a r)
               (ex vs t a))))))

(defn handle-clause [as]
  (fn [[p & exprs]]
    (let [pas (partition 2 (interleave p as))]
      (ex* pas exprs #{}))))

(defn handle-clauses [t as cs]
  `(~t
    ~@(map list (map (handle-clause as) cs))))

(defn defnm [t n as & cs]
  (if-let [tabled? (-> n meta :tabled)]
    `(def ~n
          (logos.tabled/tabled [~@as]
                               ~(handle-clauses t as cs)))
    `(defn ~n [~@as]
       ~(handle-clauses t as cs))))

;; -----------------------------------------------------------------------------
;; quick tests

(comment
  (defne appendo [x y z]
    ([() _ y])
    ([[?a . ?d] _ [?a . ?r]] (appendo ?d y ?r)))

  (defne test1 [x y]
    ([() _]))

  (defne test2 [x y]
    ([[_ _ ?a] _] (foo) (bar)))

  (defn test-match [x y]
    (matche [x y]
      ([() _])
      ([[?a . ?b] [?c ?d]] (testo ?a ?d))))
  )