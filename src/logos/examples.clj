(ns logos.examples
  (:refer-clojure :exclude [reify inc == take])
  (:use logos.minikanren))

(run* [q]
      (== q true))

;; wow nearly twice as fast!
;; 3 million unifications a second
(comment
 (dotimes [_ 10]
   (time
    (dotimes [_ 3e6]
      (run* [q]
            (== q true)))))
 )

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
  ;; 1.3s under goals branch
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (run* [r]
             (exist [x y]
                    (cond-e
                     ((teacup-o x) (== true y) s#)
                     ((== false x) (== true y)))
                    (== (cons x (cons y ())) r))))))
 )

(comment
  (defmulti multi-test type)
  (defmethod multi-test ::a [x])
  (defmethod multi-test ::b [x])

  ;; multimethods are almost twice as fast
  ;; can we get very close to that?
  ;; perhaps via tabling
  (dotimes [_ 10]
    (let [x (with-meta {} {:type ::a})]
     (time
      (dotimes [_ 8e6]
        (multi-test x)))))
  )
