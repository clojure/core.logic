(ns logos.nonrel
  (:refer-clojure :exclude [reify ==])
  (:use logos.minikanren))

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

;; TODO: tests

(defmacro cond-a [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (first
        (mplus* ~@(bind-cond-e-clauses a clauses))))))

;; TODO: test

(defmacro cond-u [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (first
        (first
         (mplus* ~@(bind-cond-e-clauses a clauses)))))))

(comment
  (run* [x]
    (cond-a
      ((== olive x) s*)
      ((== olive x) s*)
      (else u*)))
  )