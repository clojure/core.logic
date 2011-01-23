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

(defmacro cond-a [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (first
        (mplus* ~@(bind-cond-e-clauses a clauses))))))

(defmacro cond-u [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (first
        (first
         (mplus* ~@(bind-cond-e-clauses a clauses)))))))

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