(ns logos.unify
  (:use [clojure.walk :only [postwalk]])
  (:require [logos.minikanren :as mk]))

(defn lvarq-sym? [s]
  (= (first (str s)) \?))

(defn rem-? [s]
  (symbol (apply str (drop 1 (str s)))))

(defn replace-lvar [expr]
  (cond
   (lvarq-sym? expr) (mk/lvar (rem-? expr))
   :else expr))

;; TODO: replace postwalk with something much faster ?
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
    (unifier' '(?x ?y 3) '(1 2 ?z))
    (unifier' '[?x ?y] [1 2])
    (unifier' '{x ?y a 2} '{x 1 a ?b})
    
    ;; FIXME: sets do not have an order nor hashmap
    ;; we need a special unify case for them
    (unifier' '#{?x ?y 3} '#{1 2 ?z})
    
    ;; NOTE: Not supported at the moment while I iron some
    ;; other things out
    (unifier' '(?x ?y ?z ?&r) '(1 2 3 4 5 6 7 8 9 0))
    (unifier' '(?x ?y [?&a] ?&b) '(1 2 [3 4 5 6 7] 8 9 0))

    (def json-path (prep '{:foo ?x
                           :bar {:baz [?y '#{valid}]}}))

    (unifier json-path {:foo 1
                        :bar {:baz [:correct '#{valid}]}})

    (unifier json-path {:foo 1
                        :bar {:baz [:incorrect false]}})
)