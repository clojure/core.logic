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

(comment
    (unifier' '(?x ?y) '(1 2))
    (unifier' '[?x ?y] [1 2])
    (unifier' '{?x ?y} {1 2})
    (unifier' '#{?x ?y} #{1 2})
    
    ;; Not supported at the moment while I iron some
    ;; other things out
    (unifier' '(?x ?y ?z ?&r) '(1 2 3 4 5 6 7 8 9 0))
    (unifier' '(?x ?y [?&a] ?&b) '(1 2 [3 4 5 6 7] 8 9 0))
)