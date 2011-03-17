(ns logos.match
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren)
  (:require [logos.logic :as logic]
            [clojure.set :as set])
  (:import [logos.minikanren Substitutions]))

(defn lcons-p? [p]
  (not (nil? (some #{'.} p))))

(defn p->term [p]
  (cond
   (= p '_) '_
   (symbol? p) p
   (lcons-p? p) `(llist
                  ~@(map p->term
                         (remove #(contains? '#{.} %) p)))
   :else `[~@(map p->term p)]))

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

(defn ex* [[[p a] & par] expr seen]
  (let [t    (p->term p)
        vs   (extract-vars p seen)
        seen (reduce conj seen vs)]
    (cond
     (= t '_) (ex* par expr seen)
     (empty? par) (if expr
                    (ex vs t a expr)
                    (ex vs t a))
     :else (ex vs t a
               (ex* par expr seen)))))

(defn handle-clause [as]
  (fn [[p & expr]]
    (let [pas (partition 2 (interleave p as))]
      (ex* pas expr #{}))))

(defn handle-clauses [as cs]
  `(cond-e
    ~@(map list (map (handle-clause as) cs))))

(defmacro defn-e [n as & cs]
  `(defn ~n [~@as]
     ~(handle-clauses as cs)))

(defmacro match-e [xs & cs]
  (handle-clauses xs cs))

(comment
  ;; _ is deal with one of two way
  ;; 1) top-level, just skip
  ;; 2) in pattern, unify-lcons

  (defn-e append-o [x y z]
    ([() _ y])
    ([[?a . ?d] _ [?a . ?r]] (append-o ?d y ?r)))

  (run* [q]
      (append-o '(1 2) '(3 4) q))
  )