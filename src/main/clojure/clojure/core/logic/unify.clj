(ns clojure.core.logic.unify
  (:use [clojure.walk :only [postwalk]]
        clojure.set)
  (:require [clojure.core.logic.minikanren :as mk]))

(defn lvarq-sym? [s]
  (and (symbol? s) (= (first (str s)) \?)))

(defn rem-? [s]
  (symbol (apply str (drop 1 (str s)))))

(defn replace-lvar [store]
  (fn [expr]
    (cond
     (lvarq-sym? expr)
     (let [v (if-let [u (@store expr)]
               u
               (mk/lvar (rem-? expr)))]
       (swap! store conj [expr v])
       v)
     :else expr)))

(defn prep [expr]
  (let [lvars (atom {})]
    (with-meta
      (postwalk (replace-lvar lvars) expr)
      {:lvars @lvars})))

(defn unifier* [u w]
  (first
   (mk/run* [q]
            (mk/== u w)
            (mk/== u q))))

(defn binding-map* [u w]
  (let [lvars (merge (-> u meta :lvars)
                     (-> w meta :lvars))
        s (mk/unify mk/empty-s u w)]
    (when s
      (into {} (map (fn [[k v]]
                      [k (mk/walk s v)])
                    lvars)))))

(defn unifier [u w]
  (let [up (prep u)
        wp (prep w)]
    (unifier* up wp)))

(defn binding-map [u w]
  (let [up (prep u)
        wp (prep w)]
    (binding-map* up wp)))