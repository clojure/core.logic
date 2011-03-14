(ns logos.match
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren)
  (:require [logos.logic :as logic])
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

(defn unify-p [p a]
  (cond (= p '_) nil
        (and (coll? p)
             (seq p)) `(exist [~@(extract-vars p)]
                           (== ~(unifier-term p) ~a))
        :else `(== ~p ~a)))

(defn handle-clause [a*]
  (fn [[p & ex :as c]]
    (concat (remove nil? (map unify-p p a*)) ex)))

(defn handle-clauses [as cs]
  `(cond-e
    ~@(map (handle-clause as) cs)))

(comment
  (defn-e append-o [x y z]
    ([() _ y])
    ([[?a . ?d] _ [?a . ?r]] (append-o ?d y ?r)))

  (defn-e append-o [x y z]
    ([() _ y])
    ([[?a . ?d] _ [?a . ?r]] (append-o ?d y ?r)))

  (defn-e append [x y z]
    ([() _ y])
    ([[~a & ~d] _ [~a & ~r]] (append d y r)))

  ;; hmm caps plus the pipe operator?
  ;; so much to think about
  (defn-e append [x y z]
    ([() _ y])
    ([[A|D] _ [A|R]] (append D y R)))

  (match-e x
    ([()] ...)
    ([?a & ?d] ...))
  )