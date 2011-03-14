(ns logos.match
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren)
  (:import [logos.minikanren Substitutions]))

(defmacro defn-e [n as & cs]
  ~(defn ~n [~@as] (handle-clauses as cs)))

(defmacro match-e [e & cs]
  `(let [t e]
     (handle-clauses t ~@cs)))

(defn handle-clause [a*]
  (fn [[p & ex :as c]]
    (map unify-p p a*)))

(defn handle-clauses [as cs]
  `(cond-e
    ~@(map (handle-clause as) c)))

(comment
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