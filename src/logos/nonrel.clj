(ns logos.nonrel
  (:refer-clojure :exclude [reify == inc])
  (:use [logos.minikanren :exclude [lvar]]
        logos.match)
  (:import [logos.minikanren Substitutions Choice]))

;; =============================================================================
;; project

(defn project-binding [s]
  (fn [var]
   `(~var (walk ~s ~var))))

(defn project-bindings [vars s]
  (reduce concat (map (project-binding s) vars)))

(defmacro project [[& vars] & goals]
  (let [a (gensym "a")]
   `(fn [~a]
      (let [~@(project-bindings vars a)]
        ((exist []
                ~@goals) ~a)))))

;; =============================================================================
;; cond-a (soft-cut), cond-u (committed-choice)
;;
;; cond-a once a line succeeds no others are tried
;; cond-u a line can succeed only one time

;; TODO : cond-a and cond-u should probably understanding logging

(defprotocol IIfA
  (if-a [b gs c]))

(defprotocol IIfU
  (if-u [b gs c]))

;; TODO : if -> when

(defmacro if-a*
  ([])
  ([[e & gs] & grest]
     `(if-a ~e [~@gs]
            ~(if (seq grest)
               `(delay (if-a* ~@grest))
               nil))))

(defmacro if-u*
  ([])
  ([[e & gs] & grest]
     `(if-u ~e [~@gs]
            ~(if (seq grest)
               `(delay (if-u* ~@grest))
               nil))))

(extend-protocol IIfA
  nil
  (if-a [b gs c]
        (when c
          (force c))))

(extend-protocol IIfU
  nil
  (if-u [b gs c]
        (when c
          (force c))))

(extend-type Substitutions
  IIfA
  (if-a [b gs c]
        (loop [b b [g0 & gr] gs]
          (if g0
            (when-let [b (g0 b)]
              (recur b gr))
            b))))

(extend-type Substitutions
  IIfU
  (if-u [b gs c]
        (loop [b b [g0 & gr] gs]
          (if g0
            (when-let [b (g0 b)]
              (recur b gr))
            b))))

(extend-type clojure.lang.Fn
  IIfA
  (if-a [b gs c]
        (inc (if-a (b) gs c))))

(extend-type clojure.lang.Fn
  IIfU
  (if-u [b gs c]
        (inc (if-u (b) gs c))))

(extend-protocol IIfA
  Choice
  (if-a [b gs c]
        (reduce bind b gs)))

;; TODO: Choice always holds a as a list, can we just remove that?
(extend-protocol IIfU
  Choice
  (if-u [b gs c]
        (reduce bind (.a ^Choice b) gs)))

(defn cond-clauses [a]
  (fn [goals]
    `((~(first goals) ~a) ~@(rest goals))))

(defmacro cond-a
  [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (if-a* ~@(map (cond-clauses a) clauses)))))

(defmacro cond-u [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (if-u* ~@(map (cond-clauses a) clauses)))))

;; =============================================================================
;; defn-u, defn-a, match-a, match-u

(defmacro defn-a [& rest]
  (apply defn-m `cond-a rest))

(defmacro defn-u [& rest]
  (apply defn-m `cond-u rest))

(defmacro match-a [xs & cs]
  (handle-clauses `cond-a xs cs))

(defmacro match-u [xs & cs]
  (handle-clauses `cond-u xs cs))

;; =============================================================================
;; copy-term

(defn copy-term [u v]
  (project [u]
    (== (walk* (build empty-s u) u) v)))

;; =============================================================================
;; lvar nonlvar

(defmacro lvar [v]
  `(fn [a#]
     (if (lvar? (walk a# ~v))
       a# nil)))

(defmacro nonlvar [v]
  `(fn [a#]
     (if (not (lvar? (walk a# ~v)))
       a# nil)))

(comment
  (run 1 [q]
       (nonlvar q))
  )