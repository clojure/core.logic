(ns logos.nonrel
  (:refer-clojure :exclude [reify ==])
  (:use logos.minikanren)
  (:import [logos.minikanren Substitutions]))

;; =============================================================================
;; Project

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
;; cond-a, cond-u

(defprotocol IIfA
  (if-a [b gs c]))

(defprotocol IIfU
  (if-u [b gs c]))

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
            (when b
              (list b))))))

(extend-type Substitutions
  IIfU
  (if-u [b gs c]
        (loop [b b [g0 & gr] gs]
          (if g0
            (when-let [b (g0 b)]
              (recur b gr))
            (when b
              (list b))))))

(extend-protocol IIfA
  clojure.lang.ISeq
  (if-a [b gs c]
        (reduce bind b gs)))

(extend-protocol IIfU
  clojure.lang.ISeq
  (if-u [b gs c]
        (reduce bind (first b) gs)))

(defn cond-clauses [a]
  (fn [goals]
    `((~(first goals) ~a) ~@(rest goals))))

(defmacro cond-a
  [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (lazy-seq
        (if-a* ~@(map (cond-clauses a) clauses))))))

(defmacro cond-u [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (lazy-seq
        (if-u* ~@(map (cond-clauses a) clauses))))))

;; =============================================================================
;; copy-term

(defn copy-term [u v]
  (project [u]
    (== (walk* (build empty-s u) u) v)))

;; =============================================================================
;; Examples

(comment
  (if-a empty-s [s#] nil)

  ;; (olive)
  (run* [x]
    (cond-a
      ((== 'olive x) s#)
      ((== 'oil x) s#)
      (u#)))

  ;; ()
  (run* [x]
    (cond-a
      ((== 'virgin x) u#)
      ((== 'olive x) s#)
      ((== 'oil x) s#)
      (u#)))

  ;; ()
  (run* [x]
    (exist (x y)
      (== 'split x)
      (== 'pea y)
      (cond-a
        ((== 'split x) (== x y))
        (s#)))
    (== true x))

  ;; (true)
  (run* [x]
    (exist (x y)
      (== 'split x)
      (== 'pea y)
      (cond-a
        ((== x y) (== 'split x))
        (s#)))
    (== true x))

  (defn not-pasta-o [x]
    (cond-a
     ((== 'pasta x) u#)
     (s#)))

  ;; (spaghetti)
  ;; FIXME
  (run* [x]
    (cond-a
     ((not-pasta-o x) u#)
     ((== 'spaghetti x))))

  ;; cond-u

  (defn teacup-o [x]
    (cond-e
     ((== 'tea x) s#)
     ((== 'cup x) s#)))

  ;; error
  ;; FIXME
  (defn once-o [g]
    (cond-u
     (g s#)
     (u#)))

  (run* [x]
     (once-o (teacup-o x)))
  )