(ns logos.unify
  (:use [clojure.walk :only [postwalk]])
  (:require [logos.minikanren :as mk]))

(defn lvarq-sym? [s]
  (= (first (str s)) \?))

(defn rest-lvarq-sym? [s]
  (let [[f s] (str s)]
    (and (= f \?) (= s \&))))

(defn rem-? [s]
  (symbol (apply str (drop 1 (str s)))))

(defn replace-lvar [expr]
  (cond
   (rest-lvarq-sym? expr) (mk/rest-lvar (rem-? expr))
   (lvarq-sym? expr) (mk/lvar (rem-? expr))
   :else expr))

(defn prep [expr]
  (postwalk replace-lvar expr))

(defn unifier [u w]
  (first
   (mk/run* [q]
         (mk/== u w)
         (mk/== u q))))

(defn unifier' [u w]
  (let [u' (prep u)
        w' (prep w)]
   (first
    (mk/run* [q]
          (mk/== u' w')
          (mk/== u' q)))))
