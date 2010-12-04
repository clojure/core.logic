(ns logos.examples
  (:refer-clojure :exclude [reify inc == take])
  (:use logos.minikanren))

(defn likes [x y]
  (cond-e
   ((== x 'john) (== y 'mary))
   ((== x 'mary) (== y 'join))))

(defn musician [x]
  (cond-e
   ((== x 'john))
   ((== x 'bob))))

(comment

  (run* [q]
        (likes q 'mary)
        (musician q)) ;; [john]

  (run* [q]
        (musician q)) ;; [john bob]

  ;; not the question is how would one go about creating
  ;; a knowledge base of facts and chaining them together?

  ;; that is it important to keep the system open?

  ;; (?- likes 'john 'mary)
  ;; (?- likes 'mary 'john)
  ;; (?- likes ?x 'mary)
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
