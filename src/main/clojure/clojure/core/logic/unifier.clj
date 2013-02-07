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
         (lvarq-sym? expr)
         (proc-lvar expr store)
        
         (seq? expr)
         (if (or lcons? (lcons-expr? expr))
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

(defn queue-constraints [s [vs c]]
  (cond
    (vector? vs)
    (queue s (unwrap (apply c (map #(lvar % false) vs))))

    (set? vs)
    (reduce (fn [s v] (queue s (unwrap (c (lvar v false))))) s vs)

    (symbol? vs)
    (queue s (unwrap (apply c (map #(lvar % false) (list vs)))))

    :else
    (throw
     (Exception.
      (str "Only symbol, set of symbols, or vector of symbols allowed "
           "on left hand side")))))

(defn -unify* [init-s u w]
  (first
    (take*
      (fn []
        ((fresh [q]
           (== u w) (== q u)
           (fn [a]
             (fix-constraints a))
           (reifyg q))
         init-s)))))

(defn unify*
  "Unify the terms ts."
  ([ts] (unify* {} ts))
  ([opts ts]
     (let [c-s (reduce queue-constraints
                 (with-meta empty-s {:reify-vars (fn [v rs] rs)})
                 (:when opts))]
       (-unify*
         (vary-meta c-s assoc :reify-vars false)
         (reduce #(-unify* c-s %1 %2) (butlast ts))
         (last ts)))))

(defn unifier*
  "Return the unifier that unifies terms ts.
  All terms in ts should prepped terms."
  ([ts] (unifier* {} ts))
  ([opts ts]
     (letfn [(-unifier* [u w]
               (let [lvars (merge
                             (-> u meta :lvars)
                             (-> w meta :lvars))
                     s (l/unify (with-meta empty-s {:reify-vars false}) u w)]
                 (when s
                   (->> lvars
                     (filter (fn [[name var]] (not= (walk s var) var)))   
                     (map (fn [[name var]] [name (-reify s var)]))
                     (into {})))))]
       (reduce -unifier* ts))))

(defn unify
  "Unify the terms ts returning a the value that represents their
   unificaiton. Will prep the terms."
  ([ts] (unify {} ts))
  ([opts ts] (unify* opts (map prep ts))))

(defn unifier
  "Return the unifier for terms ts. Will prep the terms."
  ([ts] (unifier {} ts))
  ([opts ts] (unifier* opts (map prep ts))))
