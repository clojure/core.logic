(ns logos.match
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren)
  (:import [logos.minikanren Substitutions]))

(defmacro defn-e [args & c*]
  ~(fn [~@args] (handle-clauses [~@args] ~@c*)))

(defmacro match-e [e & c*]
  `(let [t e]
     (handle-clauses t ~@c*)))

(comment
  (defn-e append-o [x y z]
    ([() _ y])
    ([[?a & ?d] _ [?a & ?r]] (append-o ?d y ?r)))

  (defn-e append-o [x y z]
    ([() _ y])
    ([[A & D] _ [A & R]] (append-o A y R)))

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