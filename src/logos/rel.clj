(ns logos.rel
  (:refer-clojure :exclude [reify == inc test])
  (:use [logos.minikanren :exclude [lvar?]]
        logos.match)
  (:require [clojure.set :as set]))

(defn lvar? [a v]
  (logos.minikanren/lvar? (walk a v)))

(defn index [tuples]
  (->> tuples
       (map (fn [[k :as t]] {k #{t}}))
       (apply merge-with
              (fn [a b] (set/union a b)))))

(defmacro defrel [name & args]
  (let [setsym (symbol (str name "-set"))
        idxsym (symbol (str name "-indexed"))]
   `(do
      (def ~setsym (atom #{}))
      (def ~idxsym (atom {}))
      (defmacro ~name [~@args]
        (defrelg '~setsym '~idxsym ~@args)))))

(defn defrelg [setsym idxsym & args]
  `(fn [a#]
     (answers a# (deref ~setsym) (deref ~idxsym) [~@args])))

;; TODO: put in a dosync

(defmacro fact [rel & tuple]
  (let [setsym (symbol (str rel "-set"))
        idxsym (symbol (str rel "-indexed"))]
   `(do
      (swap! ~setsym conj [~@tuple])
      (reset! ~idxsym (index @~setsym)))))

(defn to-stream [aseq]
  (when aseq
    (choice (first aseq)
            (fn [] (to-stream (next aseq))))))

(defn answers [a aset indexed [f & r :as t]]
  (let [aset (if (lvar? a f)
               (indexed f)
               aset)]
    (to-stream (filter (fn [cand]
                    (unify a t cand))
                  aset))))

(comment
  (defrel is a b)
  (fact is `even? `integer?)
  (fact is `integer? `number?)

  (run 1 [q]
       (is `even? q))
 )

;; we need to keep arity

;; TODO: check that fact matches specification.
