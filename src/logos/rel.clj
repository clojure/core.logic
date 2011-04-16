(ns logos.rel
  (:refer-clojure :exclude [reify == inc test])
  (:use [logos.minikanren :exclude [lvar?]]
        logos.match
        logos.tabled)
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
  (when (seq aseq)
    (choice (first aseq)
            (fn [] (to-stream (next aseq))))))

(defn answers [a aset indexed [f & r :as t]]
  (let [aset (let [v (walk a f)]
               (if (lvar? a v)
                 aset
                 (indexed v)))]
    (to-stream
     (->> aset
          (map (fn [cand]
                    (when-let [a (unify a t cand)]
                      a)))
          (remove nil?)))))

(comment
  (do
   (defrel subsumes a b)
   (fact subsumes `even? `integer?)
   (fact subsumes `integer? `number?))

  ;; TODO: it would be nice to be able to extend a goal, redefinition
  ;; complicates things tho.

  (def is
    (tabled [x y]
       (cond-e
        ((subsumes x y))
        ((exist [z]
                (subsumes x z)
                (is z y))))))

  (run 1 [q]
       (is `even? q))

  (run* [q]
        (is `even? q))

  ;; 200ms
  ;; ~160ms w/ tabling
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (doall
        (run* [q]
              (is `even? q))))))

  (do
   (fact subsumes :puppy :young)
   (fact subsumes :puppy :dog)
   (fact subsumes :puppy :cute))

  (run* [q]
        (subsumes :puppy q))

  ;; 1 -> 3 possibilities
  ;; 1.3s for 100000, not bad
  ;; ~1.05s, when indexed
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (doall
        (run* [q]
              (subsumes :puppy q))))))

  ;; ah we definitely want negation
  (do
    (defrel man p)

   (fact man 'Bob)
   (fact man 'John)
   (fact man 'Ricky)

   (defrel woman p)
   (fact woman 'Mary)
   (fact woman 'Martha)
   (fact woman 'Lucy)
  
   (defrel likes p1 p2)
   (fact likes 'Bob 'Mary)
   (fact likes 'John 'Martha)
   (fact likes 'Ricky 'Lucy)

   (defrel fun p)
   (fact fun 'Martha))

  ;; if fun comes first ok
  ;; if it comes after likes, doesn't work
  ;; FIXME
  (run* [q]
        (exist [x y]
               (fun y)
               (likes x y)
               (== q [x y])))

  ;; ERG
  ;; trace-lvars here
  (run-debug* [q]
        (exist [x y]
               (likes x y)
               (fun y)
               (== q [x y])))

  ;; works
  (run-debug* [q]
        (exist [x y]
               (fun y)
               (== q y)))
 )
