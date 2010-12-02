(ns logos.logic
  (:refer-clojure :exclude [reify inc == take])
  (:use logos.minikanren))

(defn null-o [a]
  (== '() a))

;; TODO: enforce that d be either a rest-lvar or a seqable type
;; including vectors (which return false for seq?)

(defn cons-o [a d l]
  (== (lcons a d) l))

(defn first-o [a l]
  (exist [d]
    (cons-o a d l)))

;; TODO: enforce same constraint as for cons-o

(defn rest-o [l d]
  (exist [a]
    (== (lcons a d) l)))

;; NOTE: something to consider, whether we should implement a
;; proper pairs representation?

(defn pair-o [p]
  (exist [a d]
    (== (lcons a d) p)))

(defn twin-o [p]
  (exist [x]
         (cons-o x x p)))

;; this introduces many rest vars
(defn append-o [l s out]
  (cond-e
   ((null-o l) (== s out))
   ((exist [a d res]
           (cons-o a d l)
           (append-o d s res)
           (cons-o a res out)))))

(comment
  ;; _.0
  (run* [q]
        (== q q))

  ;; '()
  (run* [q]
        (== q '())
        (null-o q))

  ;; FAIL
  (run* [q]
        (exist [a d]
               (cons-o a d '())
               (== (cons a d) q)))

  ;; (3 4)
  (run* [q]
        (== q '(3 4)))

  ;; FAIL
  (run* [q]
        (== [q] nil))

  ;; (a)
  (run* [q]
        (cons-o 'a nil q))

  ;; (a d)
  (run* [q]
        (cons-o 'a '(d) q))

  ;; (), empty list
  (run* [q]
        (cons-o 'a q '(a)))

  ;; a
  (run* [q]
        (cons-o q '(b c) '(a b c)))

  ;; 1
  (run* [q]
        (exist [d]
         (cons-o q d '(1 2))))

  ;; 1
  (run* [q]
        (first-o q '(1 2)))

  ;; (_.0 1 2)
  (run* [q]
        (rest-o q '(1 2)))

  ;; (_.0 1 2)
  (run* [q]
        (rest-o q [1 2]))

  ;; (2)
  (run* [q]
        (rest-o [1 2] q))

  ;; (2 3 4 5 6 7 8)
  (run* [q]
        (rest-o [1 2 3 4 5 6 7 8] q))

  ;; (_.0 . _.1)
  (run* [q]
        (exist [a d]
               (cons-o a d q)))

  ;; (_.0 _.1)
  (run* [q]
        (pair-o q))

  ;; (_.0 _.1)
  (run* [q]
        (pair-o q))

  ;; (_.0 _.0)
  (run* [q]
        (twin-o q))

  ;; '(1 2 3 4)
  (run* [q]
        (append-o '(1 2) '(3 4) q))

  (run* [q]
        (append-o '(cake) '(tastes yummy) q))

  (run* [q]
        (exist [y]
               (append-o (list 'cake 'with 'iced y) '(tastes yummy) q)))

  (run* [q]
        (exist [y]
               (append-o '(cake with iced cream) y q)))

  ;; FIXME: singleton `(y) should not be allowed
  (run* [x]
        (exist [y]
               (append-o `(~y) '(d t) x)))

  (run 5 [x]
       (exist [y]
              (append-o (list 'cake y) '(d t) x)))

  ;; FIXME: trailing empty list, erg
  (run 5 [x]
       (exist [y]
              (append-o (list 'cake 'with 'ice y)
                        (list 'd 't y)
                        x)))

  ;; miniKanren under Racket beats us here
  ;; need to look into this
  ;; ~1.4 vs ~2.3s
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (run* [q]
             (append-o '(1 2) '(3 4) q)))))
  )
