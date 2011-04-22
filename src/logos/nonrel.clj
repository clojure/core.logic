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
;; conda (soft-cut), condu (committed-choice)
;;
;; conda once a line succeeds no others are tried
;; condu a line can succeed only one time

;; TODO : conda and condu should probably understanding logging

(defprotocol IIfA
  (ifa [b gs c]))

(defprotocol IIfU
  (ifu [b gs c]))

;; TODO : if -> when

(defmacro ifa*
  ([])
  ([[e & gs] & grest]
     `(ifa ~e [~@gs]
            ~(if (seq grest)
               `(delay (ifa* ~@grest))
               nil))))

(defmacro ifu*
  ([])
  ([[e & gs] & grest]
     `(ifu ~e [~@gs]
            ~(if (seq grest)
               `(delay (ifu* ~@grest))
               nil))))

(extend-protocol IIfA
  nil
  (ifa [b gs c]
        (when c
          (force c))))

(extend-protocol IIfU
  nil
  (ifu [b gs c]
        (when c
          (force c))))

(extend-type Substitutions
  IIfA
  (ifa [b gs c]
        (loop [b b [g0 & gr] gs]
          (if g0
            (when-let [b (g0 b)]
              (recur b gr))
            b))))

(extend-type Substitutions
  IIfU
  (ifu [b gs c]
        (loop [b b [g0 & gr] gs]
          (if g0
            (when-let [b (g0 b)]
              (recur b gr))
            b))))

(extend-type clojure.lang.Fn
  IIfA
  (ifa [b gs c]
        (inc (ifa (b) gs c))))

(extend-type clojure.lang.Fn
  IIfU
  (ifu [b gs c]
        (inc (ifu (b) gs c))))

(extend-protocol IIfA
  Choice
  (ifa [b gs c]
        (reduce bind b gs)))

;; TODO: Choice always holds a as a list, can we just remove that?
(extend-protocol IIfU
  Choice
  (ifu [b gs c]
        (reduce bind (.a ^Choice b) gs)))

(defn cond-clauses [a]
  (fn [goals]
    `((~(first goals) ~a) ~@(rest goals))))

(defmacro conda
  [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (ifa* ~@(map (cond-clauses a) clauses)))))

(defmacro condu [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (ifu* ~@(map (cond-clauses a) clauses)))))

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