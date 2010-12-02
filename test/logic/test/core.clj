(ns logos.test.core
  (:use [logos.minikanren] :reload)
  (:use [clojure.test]))

(lvar 'x)

(let [[x y z] (map lvar '[x y z])
      s (to-s [[x 5] [y x]])]
  s)

(let [[x y z] (map lvar '[x y z])
      s (to-s [[x 5] [y x]])]
  (ext s z y))

;; 5
(let [x  (lvar 'x)
      y  (lvar 'y)
      ss (to-s [[x 5] [y x]])]
  (lookup ss y))

;; 5
(let [[x y z c b a :as s] (map lvar '[x y z c b a])
      ss (to-s [[x 5] [y x] [z y] [c z] [b c] [a b]])]
  (lookup ss a))

;; 5, degenerate case
(let [[x m y n z o c p b q a] (map lvar '[x m y n z o c p b q a])
      ss (to-s [[x 5] [m 0] [m 0] [m 0] [m 0]
                [y x] [n 0] [n 0] [n 0] [n 0]
                [z y] [o 0] [o 0] [o 0] [o 0]
                [c z] [p 0] [p 0] [p 0] [p 0]
                [b c] [q 0] [q 0] [q 0] [q 0]
                [a b]])]
  (lookup ss a))

;; _.2
(let [x  (lvar 'x)
      y  (lvar 'y)]
  (reify-lvar-name (to-s [[x 5] [y x]])))

;; (<lvar:x> <lvar:y>)
(let [x  (lvar 'x)
      y  (lvar 'y)]
  (lookup (to-s [[x 5] [y x]]) `(~x ~y)))

;; (5 5)
(let [x  (lvar 'x)
      y  (lvar 'y)]
  (reify-lookup (to-s [[x 5] [y x]]) `(~x ~y)))

;; (5 _.0 (true _.1 _.0) _.2)
(let [[x y z] (map lvar '[x y z])
      v `(5 ~x (true ~y ~x) ~z)
      r (reify empty-s v)]
  r)

(run* [q]
      (== true q))

(run* [q]
      succeed
      (== true q))

(run* [q]
      fail
      (== true q))

(run* [q]
      (== false q)
      (== true q))

;; [[1 5]]
(run* [q]
      (exist [x y]
             (== [x y] [1 5])
             (== [x y] q)))

;; [[1 5]]
(run* [q]
      (exist [x y]
             (== {x y} {1 5})
             (== [x y] q)))

;; [[_.0 _.1]]
(run* [q]
      (exist [x y]
             (== [x y] q)))

(run* [q]
      (exist [x y]
             (== {x y} q)))

(run* [q]
      (exist [x y z]
             (== y z)
             (== [1 2 {x y}] q)
             (== z 5)))

(let [[x y q] (map lvar '[x y q])
      s (unify empty-s {(lvar 'x) (lvar 'y)} q)]
  (reify s q))

(run* [q]
      (exist [x y r]
             (== {x y} r)
             (== {x y} {1 5})
             (== r q)))

(run* [q]
      (exist [x y z]
             (== y z)
             (== [1 2 {x y}] q)
             (== z 5)))

(run* [q]
      (exist [x y z]
             (== y z)
             (== [1 2 #{x y}] q)
             (== z 5)))

(run* [x]
      (cond-e
       ((== x 'olive) succeed)
       (succeed succeed)
       ((== x 'oil) succeed)))

(run* [r]
      (exist [x y]
             (cond-e
              ((== 'split x) (== 'pea y))
              ((== 'navy x) (== 'bean y)))
             (== (cons x (cons y ())) r)))

(defn teacup-o [x]
  (cond-e
   ((== 'tea x) s#)
   ((== 'cup x) s#)))

(defn teacup-o [x]
  (cond-e
   ((== 'tea x) (trace-lvars "teacup-o" x) s#)
   ((== 'cup x) (trace-lvars "teacup-o" x) s#)
   ((== 'time x))))

;; the return order is interesting
;; interleaving
(run* [r]
      (exist [x y]
             (cond-e
              ((teacup-o x) (== true y) s#)
              ((== false x) (== true y)))
             (== (cons x (cons y ())) r)))

(run 1 [r]
     (exist [x y]
            (cond-e
             ((teacup-o x) (== true y) s#)
             ((== false x) (== true y)))
            (== (cons x (cons y ())) r)))

(defn nil-o [a]
  (== nil a))

(defn cons-o [a d l]
  (== (cons a d) l))

(defn append-o [l s out]
  (cond-e
   ((nil-o l) (== s out))
   ((exist [a &d &res]
           (cons-o a &d l)
           (append-o &d s &res)
           (cons-o a &res out)))))

(let [x (lvar 'x)
      y (lvar 'y)
      v [x y]]
  (run* [q]
        (== v q)))

;; attempt to understand the above
(take
 false
 (fn []
   ((fn [a__10796__auto__]
      (fn []
        (let [x (lvar 'x)]
          (bind
           ((fn [a10883]
              (fn []
                (mplus
                 (bind ((fn [a__10763__auto__]
                          (if-let [s__10764__auto__ (unify a__10763__auto__ x 'olive)]
                            s__10764__auto__
                            false)) a10883) succeed)
                 (fn [] (bind ((fn [a__10763__auto__]
                                 (if-let [s__10764__auto__ (unify a__10763__auto__ x 'oil)]
                                   s__10764__auto__
                                   false)) a10883) succeed)))))
            a__10796__auto__)
           (fn [a__10816__auto__] (conj [] (reify a__10816__auto__ x)))))))
    empty-s)))

(run* [q]
      (exist [&q]
             (== &q q)))

(run* [q]
      (exist [&r]
             (== `(1 ~&r) '(1 2 3 4 5))
             (== &r q)))

(unifier' '(?x ?y) '(1 2))
(unifier' '(?x ?y ?z ?&r) '(1 2 3 4 5 6 7 8 9 0))
(unifier' '(?x ?y [?&a] ?&b) '(1 2 [3 4 5 6 7] 8 9 0))

(run* [&q]
      (== &q &q))

(run* [&q]
      (== q '())
      (null-o q))

;; ()
(run* [q]
      (== q '())
      (null-o q))

(run* [&q]
      (== &q '())
      (null-o &q)
      (null-o &q))

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

;;
(run* [&q]
      (exist [a &d]
             (cons-o a &d &q)))

;; (_.0 _.&1)
(run* [q]
      (pair-o q))

;; (_.0 _.&1)
(run* [&q]
      (pair-o &q))

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
      (exist [&y]
             (append-o '(cake with iced cream) &y q)))

;; FIXME: singleton `(&y) should not be allowed
(run* [x]
      (exist [&y]
             (append-o `(~&y) '(d t) x)))

(run 5 [x]
     (exist [&y]
            (append-o (list 'cake &y) '(d t) x)))

;; FIXME: trailing empty list, erg
(run 5 [x]
     (exist [&y]
            (append-o (list 'cake 'with 'ice &y)
                      (list 'd 't &y)
                      x)))
