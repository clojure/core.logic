(ns clojure.core.logic.unifier
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic.protocols]
        [clojure.core.logic :exclude [unify] :as l]))

;; =============================================================================
;; Easy Unification

(defn- lvarq-sym? [s]
  (and (symbol? s) (= (first (str s)) \?)))

(defn- proc-lvar [lvar-expr store]
  (let [v (if-let [u (@store lvar-expr)]
            u
            (lvar lvar-expr false))]
    (swap! store conj [lvar-expr v])
    v))

(defn- lcons-expr? [expr]
  (and (seq? expr) (some '#{.} (set expr))))

(declare prep*)

(defn- replace-lvar [store]
  (fn [expr]
    (if (lvarq-sym? expr)
      (proc-lvar expr store)
      (if (lcons-expr? expr)
        (prep* expr store)
        expr))))

(defn- prep*
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
                          (lcons (prep* f store) tail)))
                      (doall (walk-term expr (replace-lvar store))))
        :else expr))))

(defn prep
  "Prep a quoted expression. All symbols preceded by ? will
  be replaced with logic vars."
  [expr]
  (let [lvars (atom {})
        prepped (if (lcons-expr? expr)
                  (prep* expr lvars true)
                  (doall (walk-term expr (replace-lvar lvars))))]
    (with-meta prepped {:lvars @lvars})))

(defn unify*
  "Unify the terms u and w."
  ([u w]
     (let [init-s (reduce
                    (fn [s [vs cs]]
                      (let [vs (if (seq? vs) vs (list vs))]
                        (queue s (unwrap (apply cs (map #(lvar % false) vs))))))
                    (with-meta empty-s {:reify-vars false}) (-> u meta ::when))]
       (first
         (take*
           (fn []
             ((fresh [q]
                (== u w) (== q u)
                (fn [a]
                  (fix-constraints a))
                (reifyg q))
              init-s))))))
  ([u w & ts]
     (if (some #{:when} ts)
       (let [terms (take-while #(not= % :when) ts)
             constraints (last ts)]
         (reduce #(unify* %1 %2)
           (unify* (vary-meta u assoc ::when constraints) w)
           terms))
       (apply unify* (unify* u w) ts))))

(defn unifier*
  "Return the binding map that unifies terms u and w.
  u and w should prepped terms."
  ([u w]
     (let [lvars (merge (-> u meta :lvars)
                        (-> w meta :lvars))
           s (l/unify (with-meta empty-s {:reify-vars false}) u w)]
       (when s
         (->> lvars
           (filter (fn [[name var]] (not= (walk s var) var)))   
           (map (fn [[name var]] [name (-reify s var)]))
           (into {})))))
  ([u w & ts]
     (apply unifier* (unifier* u w) ts)))

(defn unify
  "Unify the terms u and w. Will prep the terms."
  ([u w]
     {:pre [(not (lcons? u))
            (not (lcons? w))]}
     (let [up (vary-meta (prep u) merge (meta u))
           wp (prep w)]
       (unify* up wp)))
  ([u w & ts]
     (if (some #{:when} ts)
       (let [terms (take-while #(not= % :when) ts)
             constraints (last ts)]
         (reduce #(unify %1 %2)
           (unify (vary-meta u assoc ::when constraints) w)
           terms))
       (apply unify (unify u w) ts))))

(defn unifier
  "Return the binding map that unifies terms u and w.
  Will prep the terms."
  ([u w]
     {:pre [(not (lcons? u))
            (not (lcons? w))]}
     (let [up (prep u)
           wp (prep w)]
       (unifier* up wp)))
  ([u w & ts]
     (apply unifier (unifier u w) ts)))
