(ns logos.unify
  (:use [clojure.walk :only [postwalk]]
        clojure.set)
  (:require [logos.minikanren :as mk]))

(defn lvarq-sym? [s]
  (= (first (str s)) \?))

(defn rem-? [s]
  (symbol (apply str (drop 1 (str s)))))

(defn replace-lvar [store]
  (fn [expr]
   (cond
    (lvarq-sym? expr)
      (let [v (mk/lvar (rem-? expr))]
        (swap! store conj v)
        v)
    :else expr)))

;; TODO: replace postwalk with something much faster ?
(defn prep [expr]
  (let [lvars (atom #{})]
    (with-meta
      (postwalk (replace-lvar lvars) expr)
      {:lvars @lvars})))

(defn unifier [u w]
  (first
   (mk/run* [q]
         (mk/== u w)
         (mk/== u q))))

(defn binding-map [u w]
  (let [r (atom nil)
        lvars (union (-> u meta :lvars)
                     (-> w meta :lvars))
        s (mk/unify mk/empty-s u w)]
    (into {} (map (fn [lvar]
                    [lvar (mk/walk s lvar)])
              lvars))))

(defn unifier' [u w]
  (let [u' (prep u)
        w' (prep w)]
   (unifier u' w')))

(defn binding-map' [u w]
  (let [u' (prep u)
        w' (prep w)]
    (binding-map u w)))

(comment
    (unifier' '(?x ?y) '(1 2))
    (unifier' '(?x ?y 3) '(1 2 ?z))
    (unifier' '[?x ?y] [1 2])
    (unifier' '{x ?y a 2} '{x 1 a ?b})
    
    ;; NOTE: Not supported at the moment while I iron some
    ;; other things out
    (unifier' '(?x ?y ?z ?&r) '(1 2 3 4 5 6 7 8 9 0))
    (unifier' '(?x ?y [?&a] ?&b) '(1 2 [3 4 5 6 7] 8 9 0))

    ;; TODO: look into this
    (def json-path1 (prep '{:foo ?x
                           :bar {:baz [?y '#{valid}]}}))

    (unifier json-path1 {:foo 1
                         :bar {:baz [:correct '#{valid}]}})

    (unifier json-path1 {:foo 1
                         :bar {:baz [:incorrect false]}})

    ;; TODO: use the original name in the binding map
    ;; TODO: make sure that using the same name doesn't cause problems
    (def json-path2 (prep '{:foo ?x
                           :bar {:baz [?y ?z]}}))

    (unifier json-path2 {:foo "A val 1"
                         :bar {:baz ["A val 2" "A val 3"]}})

    (binding-map json-path2 {:foo "A val 1"
                             :bar {:baz ["A val 2" "A val 3"]}})
)