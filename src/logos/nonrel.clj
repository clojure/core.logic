(ns logos.nonrel
  (:refer-clojure :exclude [reify ==])
  (:use logos.minikanren))

;; from jduey
(defmacro cond-a [& clauses]
 (let [a (gensym "a")]
   `(fn [~a]
      (first
       (mplus* ~@(bind-cond-e-clauses a clauses))))))
