(ns logos.scratch
  (:refer-clojure :exclude [reify == inc test])
  (:use logos.minikanren
        logos.match))

(defn index [tuples]
  (into {}
   (map (fn [[k & rest]]
          [k rest])
        tuples)))

(defmacro defrel [name]
  `(defonce  ~name (atom #{})))

(defmacro fact [rel & tuple]
  `(swap! ~rel conj [~@tuple]))
