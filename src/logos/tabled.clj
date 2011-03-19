(ns logos.tabled
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren))

(defmacro tabled []
  `(let [cache# (atom {})]
     (fn [a#]
       )))