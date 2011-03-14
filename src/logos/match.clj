(ns logos.match
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren)
  (:require [logos.logic :as logic]
            [clojure.set :as set])
  (:import [logos.minikanren Substitutions]))

(defmacro defn-e [n as & cs]
  `(defn ~n [~@as] ~(handle-clauses as cs)))

(defmacro match-e [e & cs]
  `(let [t e]
     (handle-clauses t ~@cs)))

(defn extract-vars [s]
  (set (remove #(= '. %) (flatten s))))

(defn lcons-p? [p]
  (not (nil? (some #{'.} p))))

(defn unifier-term [t]
  (cond
   (symbol? t) t
   (lcons-p? t) `(llist ~@(map unifier-term (remove #(= % '.) t)))
   :else `[~@(map unifier-term t)]))

(defn lvar-sym? [s]
  (= (first (str s)) \?))

(defn exist-expr? [[f & r]]
  (= f `exist))

;; TODO: redesign

(defn unify-p [[fp & rp :as p] [fa & ra :as a] seen exprs]
  (if (not (nil? (seq p)))
    (let [[ex new-vars] (cond
                         (= fp '_) nil
                         (and (coll? fp)
                              (seq fp)) (let [vs (set/difference (extract-vars fp) seen)]
                                          [`(exist [~@vs]
                                                   (== ~(unifier-term fp) ~fa)) vs])
                              (and (lvar-sym? fp)
                                   (not (contains? seen fp)))
                                         [`(exist [~fp]
                                                 (== ~fp ~fa))
                                          [fp]]
                              :else      [`(exist []
                                                  (== ~fp ~fa)) []])
          r (unify-p rp ra (reduce conj seen new-vars) exprs)]
      (if r
        (if ex
          (let [r (if (exist-expr? r)
                    (list r)
                    r)]
            (concat ex r))
          r)
        ex))
    exprs))

(defn handle-clause [a*]
  (fn [[p & ex :as c]]
    (unify-p p a* #{} ex)))

(defn handle-clauses [as cs]
  `(cond-e
    ~@(map list (map (handle-clause as) cs))))

(comment
  (defn-e append-o [x y z]
    ([() _ y])
    ([[?a . ?d] _ [?a . ?r]] (append-o ?d y ?r)))

  (run* [q]
      (append-o '(1 2) '(3 4) q))
  )