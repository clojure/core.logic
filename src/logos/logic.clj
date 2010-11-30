(ns logos.logic
  (:refer-clojure :exclude [== reify])
  (:use [logos.minikanren :only
         [run run* exist reify cond-e == lvar rest-lvar
          lvar? empty-s rest-lvar? trace-lvars trace-s unify]]))

(defn nil-o [a]
  (== nil a))

(defn null-o [a]
  (== '() a))

;; TODO: enforce that d be either a rest-lvar or a seqable type
;; including vectors (which return false for seq?)

(defn cons-o [a d l]
  (== (cons a d) l))

(defn first-o [a l]
  (exist [&d]
    (cons-o a &d l)))

;; TODO: enforce same constraint as for cons-o

(defn rest-o [l d]
  (exist [a]
    (== (cons a d) l)))

;; NOTE: something to consider, whether we should implement a
;; proper pairs representation?

(defn pair-o [p]
  (exist [a &d]
    (== (cons a &d) p)))

(defn twin-o [p]
  (exist [x]
    (cons-o x `(~x) p)))

;; AHA! a clue to how things work
;; if a function, we call it with the arguments
;; which will just return something we can convert
;; back into a stream!

(defn append-o [l s out]
  (cond-e
   ((trace-lvars "== append-o" l s out))
   ((null-o l) (== s out) (trace-lvars "success"))
   ((exist [a &d &res]
           (trace-lvars "append-o 3" a &d &res)
           (cons-o a &d l)
           (trace-lvars "append-o 4" a &d &res)
           (append-o &d s &res)
           (cons-o a &res out)))))

(comment
  ;; ()
  (run* [q]
        (== q '())
        (null-o q))

  ;; FAILS, as it should
  (run* [q]
        (exist [a &d]
               (cons-o a &d '())
               (== (cons a &d) q)))

  ;; ()
  (run* [&q]
        (== &q '())
        (null-o &q))

  ;; FIXME
  (run* [&q]
        (== &q '(3 4)))

  (run* [&q]
        (exist [&l]
               (null-o &l)
               (== &q '(3 4))))
  
  ;; ()
  (run* [q]
        (null-o q))

  ;; should fail
  (run* [&q]
        (== [&q] nil))

  ;; (a)
  (run* [q]
        (cons-o 'a nil q))

  ;; (a d)
  (run* [q]
        (cons-o 'a '(d) q))

  ;; (), empty list
  (run* [&q]
        (cons-o 'a &q '(a)))

  ;; a
  (run* [q]
        (cons-o q '(b c) '(a b c)))

  ;; 1
  (run* [q]
        (exist [&d]
         (cons-o q &d [1 2])))

  ;; 1
  (run* [q]
        (first-o q [1 2]))

  ;; (_.0 1 2)
  (run* [q]
        (rest-o q [1 2]))

  ;; (2)
  (run* [&q]
        (rest-o [1 2] &q))

  ;; (2 3 4 5 6 7 8)
  (run* [&q]
        (rest-o [1 2 3 4 5 6 7 8] &q))

  ; (_.0 _.&1)
  (run* [q]
        (pair-o q))

  ; (_.0 _.&1)
  (run* [&q]
        (pair-o &q))

  ; (_.0 _.0)
  (run* [q]
        (twin-o q))

  ;; FIXME
  (run* [q]
        (append-o '(1 2) '(3 4) q))
  )
