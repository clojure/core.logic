(ns logos.match
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren)
  (:require [logos.logic :as logic]
            [clojure.set :as set])
  (:import [logos.minikanren Substitutions]))

(defn lcons-p? [p]
  (and (coll? p)
       (not (nil? (some #{'.} p)))))

(defn p->term [p]
  (cond
   (= p '_) `(lvar)
   (lcons-p? p) `(llist
                  ~@(map p->term
                         (remove #(contains? '#{.} %) p)))
   (coll? p) `[~@(map p->term p)]
   :else p))

(defn lvar-sym? [s]
  (= (first (str s)) \?))

(defn extract-vars
  ([p]
     (cond
      (lvar-sym? p) #{p}
      (coll? p) (set (filter lvar-sym? (flatten p)))
      :else #{}))
  ([p seen]
     (set/difference (extract-vars p) (set seen))))

(defn ex
  ([vs t a]
     `(exist [~@vs]
             (== ~t ~a)))
  ([vs t a expr]
     `(exist [~@vs]
             (== ~t ~a)
             ~expr)))

(defn ex* [[[p a :as pa] & par] expr seen]
  (let [t    (p->term p)
        vs   (extract-vars p seen)
        seen (reduce conj seen vs)]
    (cond
     (nil? pa) expr
     (= p '_) (ex* par expr seen)
     (empty? par) (if expr
                    (ex vs t a expr)
                    (ex vs t a))
     :else (let [r (ex* par expr seen)]
             (if r
               (ex vs t a r)
               (ex vs t a))))))

(defn handle-clause [as]
  (fn [[p & expr]]
    (let [pas (partition 2 (interleave p as))]
      (ex* pas (first expr) #{}))))

(defn handle-clauses [as cs]
  `(cond-e
    ~@(map list (map (handle-clause as) cs))))

(defmacro defn-e [n as & cs]
  `(defn ~n [~@as]
     ~(handle-clauses as cs)))

(defmacro match-e [xs & cs]
  (handle-clauses xs cs))

(comment
  (defn-e append-o [x y z]
    ([() _ y])
    ([[?a . ?d] _ [?a . ?r]] (append-o ?d y ?r)))

  (defn-e test-o [x y]
    ([() _]))

  (defn-e test-2-o [x y]
    ([[_ _ ?a] _]))

  (defn test-o [x y]
    (match-e [x y]
       ([() _])
       ([[?a . ?b] [?c ?d]] (test-o ?a ?d))))
  )