(ns logos.match
  (:refer-clojure :exclude [reify ==])
  (:use logos.minikanren)
  (:import [logos.minikanren Substitutions]))

(defmacro defn-e [args c*]
  ~(fn [~@args] (handle-clauses [~@args] ~@c*)))

(defmacro match-e [e c & c*]
  `(let [t e]
     (handle-clauses t ~@c*)))

(comment
  (defn-e append [x y z]
    ([() _ y])
    ([[?a & ?d] _ [?a & ?r]] (append d y r)))

  (match-e x
    ([()] ...)
    ([?a & ?d] ...))
  )