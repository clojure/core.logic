(ns logos.examples
  (:refer-clojure :exclude [reify inc == take])
  (:use logos.minikanren))

(defn likes
  [x y]
  (cond-e
   ((== x 'john) (== y 'mary))
   ((== x 'mary) (== y 'john))))

(defn musician [x]
  (cond-e
   ((== x 'john))
   ((== x 'bob))))

(comment

  (run* [q]
        (likes q 'mary)
        (musician q))
  
  ;; [john]

  (run* [q]
        (musician q))

  (run* [q]
        (exist [x y]
               (likes x y)
               (== (cons x (cons y '())) q)))
  
  ;; [john bob]

  ;; this is already useful of course

  ;; not the question is how would one go about creating
  ;; a knowledge base of facts and chaining them together
  ;; in an open manner?

  ;; (?- likes 'john 'mary)
  ;; first time, creates a function likes of two arities
  ;; 2 argument, and 3 argument one that takes a next
  ;; parameter
  
  ;; (?- likes 'mary 'john)
  ;; (?- likes ?x 'mary)

  ;; chained goals?
  )

(defn likes
  ([x y]
     (likes x y u#))
  ([x y next]
     (cond-e
      ((== x 'john) (== y 'mary))
      ((== x 'mary) (== y 'join))
      (next))))


(defn g1 [x g2]
  (cond-e
   ((== x 'foo))
   (g2)))

;; interesting we can pass goals as parameters
(run* [q]
      (g1 q (== q 'foo)))

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
