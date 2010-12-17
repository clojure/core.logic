(ns logos.logic
  (:refer-clojure :exclude [reify ==])
  (:use logos.minikanren))

(defn null-o [a]
  (== '() a))

(defn cons-o [a d l]
  (== (lcons a d) l))

(defn first-o [l a]
  (exist [d]
    (cons-o a d l)))

(defn rest-o [l d]
  (exist [a]
    (== (lcons a d) l)))

(defn pair-o [p]
  (exist [a d]
    (== (lcons a d) p)))

(defn twin-o [p]
  (exist [x]
         (cons-o x x p)))

(defn append-o [l s out]
  (cond-e
   ((null-o l) (== s out))
   ((exist [a d res]
           (cons-o a d l)
           (cons-o a res out)
           (append-o d s res)))))

(defn flatten-o [s out]
  (cond-e
   ((null-o s) (== '() out))
   ((pair-o s)
    (exist [a d res-a res-d]
           (cons-o a d s)
           (flatten-o a res-a)
           (flatten-o d res-d)
           (append-o res-a res-d out)))
   ((cons-o s '() out))))

(defn member-o [x l]
  (cond-e
   ((first-o l x))
   ((exist [r]
           (rest-o l r)
           (member-o x r)))))

(comment
  ;; just shows all the members
  (run* [q]
        (member-o q '(hummus with pita)))

  ;; sweet
  (run 5 [q]
       (member-o 'hummus q))

  ;; (_.0 . _.1)
  (run* [q]
        (pair-o q))

  ;; (_.0 _.0)
  (run* [q]
        (twin-o q))

  (run* [q]
        (exist [r]
         (cons-o r '() q)))

  ;; '(1 2 3 4)
  (run* [q]
        (append-o '(1 2) '(3 4) q))

  (def *foo* (atom []))
  (trace *foo*
         (run* [q]
               (append-o '(1 2) '(3 4) q)))

  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (run* [q]
             (append-o '(1 2) '(3 4) q)))))

  (run* [q]
        (append-o '(cake) '(tastes yummy) q))

  (run* [q]
        (exist [y]
               (append-o '(cake with iced cream) y q)))

  (run* [x]
        (exist [y]
               (append-o `(~y) '(d t) x)))

  ;; (cake d t)
  ;; (cake _.0 d t)
  ;; (cake _.0 _.1 d t)
  ;; (cake _.0 _.1 _.2 d t)
  ;; (cake _.0 _.1 _.2 _.3 d t)

  (run 5 [x]
       (exist [y]
              (append-o (llist 'cake y) '(d t) x)))

  ;; works
  (run 5 [x]
       (exist [y]
              (append-o (llist 'cake y) '(t) x)))

  ;; works
  (run 5 [x]
       (exist [y]
              (append-o (llist 'cake 'with 'ice y)
                        (llist 'd 't y)
                        x)))

  ;; doens't work
  (run* [x]
        (flatten-o '[[a b] c] x))

  ;; doesn't work
  (run* [x]
        (flatten-o '((a) b) x))
  
  ;; hmm, complaint about PersistentEmptyList
  (run* [x]
        (flatten-o '((a b) c) x))

  ;; 800ms
  ;; 8s much slower than Racket now
  (dotimes [_ 10]
    (time
     (dotimes [_ 1000]
       (doall
        (run* [x]
              (flatten-o '[[a b] c] x))))))
  )

