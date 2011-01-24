(ns logos.match
  (:refer-clojure :exclude [reify ==])
  (:use logos.minikanren)
  (:import [logos.minikanren Substitutions]))

(comment
  (defn-e append [x y z]
    ([() _ y])
    ([[?a & ?d] _ [?a & ?r]] (append d y r)))

  (match-e x
    ([()] ...)
    ([?a & ?d] ...))
  )