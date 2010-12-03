(ns logos.examples
  (:use logos.minikanren))

(run* [q]
      (== q true))

;; wow nearly twice as fast!
;; 3 million unifications a second
(dotimes [_ 10]
  (time
   (dotimes [_ 3e6]
     (run* [q]
       (== q true)))))

(run* [q]
      (== q true)
      (== q false))

(defn teacup-o [x]
  (cond-e
   ((== 'tea x) s#)
   ((== 'cup x) s#)))

;; Cons cannot be cast to clojure.lang.IFn
;; sounds like something not wrapped in unit
(run* [r]
      (exist [x y]
             (cond-e
              ((teacup-o x) (== true y) s#)
              ((== false x) (== true y)))
             (== (cons x (cons y ())) r)))

(comment
  (defmulti multi-test type)
  (defmethod multi-test ::a [x])
  (defmethod multi-test ::b [x])

  ;; multimethods are almost twice as fast
  ;; can we get very close to that?
  ;; I bet we can via tabling
  (dotimes [_ 10]
    (let [x (with-meta {} {:type ::a})]
     (time
      (dotimes [_ 8e6]
        (multi-test x)))))
  )
