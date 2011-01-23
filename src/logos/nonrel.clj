(ns logos.nonrel
  (:refer-clojure :exclude [reify ==])
  (:use logos.minikanren))

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
  (if-a [b]))

(defprotocol IIfU
  (if-u [b]))

(defmacro if-a*
  ([] nil)
  ([[e & goals] & rest]
     ))

(defmacro if-u*
  ([] nil)
  ([[e & goals] & rest]
     ))

(extend-protocol IIfA
  nil
  (if-a [b & [f r]] (if-u f )))

(extend-type Substitution
  IIfA
  (if-a [b]))

(extend-type Substitution
  IIfU
  (if-u [b]))

(extend-protocol IIfA
  clojure.lang.ISeq
  (if-a [b]))

(extend-type IIfU
  clojure.lang.ISeq)

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

;; =============================================================================
;; Examples

(comment
  ;; ('olive)
  (run* [x]
    (cond-a
      ((== 'olive x) s#)
      ((== 'oil x) s#)
      (u#)))

  ;; '()
  ;; FIXME
  (run* [x]
    (cond-a
      ((== 'virgin x) u#)
      ((== 'olive x) s#)
      ((== 'oil x) s#)
      (u#)))


  ;; (true)
  ;; FIXME
  (run* [x]
    (exist (x y)
      (== 'split x)
      (== 'pea y)
      (cond-a
        ((== 'split x) (== x y))
        (s#)))
    (== true x))

  ;; cond-u
  )