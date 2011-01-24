(ns logos.match
  (:refer-clojure :exclude [reify ==])
  (:use logos.minikanren)
  (:import [logos.minikanren Substitutions]))

(comment
  (defm append [x y z]
    ([() _ y])
    ([[?a & ?d] _ [?a & ?r]] (append d y r)))
  )