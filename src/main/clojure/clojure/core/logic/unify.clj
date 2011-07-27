(ns clojure.core.logic.unify
  (:use [clojure.walk :only [postwalk]]
        clojure.set)
  (:require [clojure.core.logic.minikanren :as mk]))

(defn lvarq-sym? [s]
  (and (symbol? s) (= (first (str s)) \?)))

(defn rem-? [s]
  (symbol (apply str (drop 1 (str s)))))

(defn proc-lvar [lvar-expr store]
  (let [v (if-let [u (@store lvar-expr)]
            u
            (mk/lvar (rem-? lvar-expr)))]
    (swap! store conj [lvar-expr v])
    v))

(defn lcons-expr? [expr]
  (and (seq? expr) (some '#{.} (set expr))))

(declare prep*)

(defn replace-lvar [store]
  (fn [expr]
    (if (lvarq-sym? expr)
      (proc-lvar expr store)
      (if (lcons-expr? expr)
        (prep* expr store)
        expr))))

(defn prep*
  ([expr store] (prep* expr store false false))
  ([expr store lcons?] (prep* expr store lcons? false))
  ([expr store lcons? last?]
     (let [expr (if (and last? (seq expr))
                  (first expr)
                  expr)]
       (cond
        (lvarq-sym? expr) (proc-lvar expr store)
        (seq? expr) (if (or lcons? (lcons-expr? expr))
                      (let [[f & n] expr
                            skip (= f '.)
                            tail (prep* n store lcons? skip)]
                        (if skip
                          tail
                          (mk/lcons (prep* f store) tail)))
                      (postwalk (replace-lvar store) expr))
        :else expr))))

(defn prep [expr]
  "Prep a quoted expression. All symbols preceded by ? will
  be replaced with logic vars."
  (let [lvars (atom {})
        prepped (if (lcons-expr? expr)
                  (prep* expr lvars true)
                  (postwalk (replace-lvar lvars) expr))]
    (with-meta prepped {:lvars @lvars})))

(defn unifier* [u w]
  "Unify the terms u and w."
  (first
    (mk/run* [q]
      (mk/== u w)
      (mk/== u q))))

(defn binding-map* [u w]
  "Return the binding map that unifies terms u and w.
  u and w should prepped terms."
  (let [lvars (merge (-> u meta :lvars)
                     (-> w meta :lvars))
        s (mk/unify mk/empty-s u w)]
    (when s
      (into {} (map (fn [[k v]]
                      [k (mk/reify s v)])
                    lvars)))))

(defn unifier [u w]
  "Unify the terms u and w. Will prep the terms."
  {:pre [(not (mk/lcons? u))
         (not (mk/lcons? w))]}
  (let [up (prep u)
        wp (prep w)]
    (unifier* up wp)))

(defn binding-map [u w]
  "Return the binding map that unifies terms u and w.
  Will prep the terms."
  {:pre [(not (mk/lcons? u))
         (not (mk/lcons? w))]}
  (let [up (prep u)
        wp (prep w)]
    (binding-map* up wp)))