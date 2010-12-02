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