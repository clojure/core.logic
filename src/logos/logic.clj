(ns logos.logic
  (:refer-clojure :exclude [==])
  (:use [logos.minikanren :only
         [run run* exist cond-e == lvar? rest-lvar? trace-lvars]]))

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
   ((trace-lvars "append-o" &d s &res))
   ((nil-o l) (== s out))
   ((exist [a &d &res]
           (cons-o a &d l)
           (append-o &d s &res)
           (cons-o a &res out)))))

(comment
  ;; FIXME: (nil)
  (run* [q]
        (null-o q))

  ;; (a)
  (run* [q]
        (cons-o 'a nil q))
  
  (run* [q]
        (cons-o 'a '(d) q))

  ;; FIXME: q should be nil or empty list
  (run* [&q]
        (cons-o 'a &q '(a)))

  ;; works
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

  (run* [&q]
        (rest-o [1 2] &q))
  
  (run* [&q]
        (rest-o [1 2 3 4 5 6 7 8] &q))

  ; (_.0 _.&1)
  (run* [q]
        (pair-o q))

  ; (_.0 _.0)
  (run* [q]
        (twin-o q))

  (run* [out]
        (exist [a &d &res]
               (cons-o a &d [1 2])
               (== &d out)))

  (run* [out]
        (exist [a &d &res]
               (cons-o a &d [1 2])
               (cons-o a &res out)))

  ;; FIXME
  (run* [q]
        (append-o '(1 2) '(3 4) q))
  )
