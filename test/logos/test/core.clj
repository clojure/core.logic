(ns logos.test.core
  (:refer-clojure :exclude [reify ==])
  (:use [logos.minikanren] :reload)
  (:use [logos.logic] :reload)
  (:use [clojure.test]))

(defn teacup-o [x]
  (cond-e
   ((== 'tea x) s*)
   ((== 'cup x) s*)))

(deftest test-basic-walk
  (is (= (let [x  (lvar 'x)
               y  (lvar 'y)
               ss (to-s [[x 5] [y x]])]
           (walk ss y))
         5)))

(deftest test-deep-walk
  (is (= (let [[x y z c b a :as s] (map lvar '[x y z c b a])
               ss (to-s [[x 5] [y x] [z y] [c z] [b c] [a b]])]
           (walk ss a))
         5)))

(deftest test-reify-lvar-name
  (is (= (let [x  (lvar 'x)
               y  (lvar 'y)]
           (reify-lvar-name (to-s [[x 5] [y x]])))
         '_.2)))

(deftest test-walk*
  (is (= (let [x  (lvar 'x)
               y  (lvar 'y)]
           (walk* (to-s [[x 5] [y x]]) `(~x ~y)))
         '(5 5))))

(deftest test-basic-unify
  (is (= (run* [q]
               (== true q))
         '(true))))

(deftest test-basic-failure
  (is (= (run* [q]
               fail
               (== true q))
         [])))

(deftest test-basic-unify-2
  (is (= (run* [q]
               (exist [x y]
                      (== [x y] [1 5])
                      (== [x y] q)))
         [[1 5]])))

(deftest test-basic-unify-3
  (is (=  (run* [q]
                (exist [x y]
                       (== [x y] q)))
         '[[_.0 _.1]])))

(deftest test-basic-unify-4
  (is (=  (run* [q]
                (exist [x y]
                       (== {x y} q)))
         '[{_.0 _.1}])))

(deftest test-basic-cond-e
  (is (=  (run* [x]
                (cond-e
                 ((== x 'olive) succeed)
                 (succeed succeed)
                 ((== x 'oil) succeed)))
          '[olive _.0 oil])))

(deftest test-basic-cond-e-2
  (is (= (run* [r]
               (exist [x y]
                      (cond-e
                       ((== 'split x) (== 'pea y))
                       ((== 'navy x) (== 'bean y)))
                      (== (cons x (cons y ())) r)))
         '[(split pea) (navy bean)])))

(deftest test-basic-conde-e-3
  (is (= (run* [r]
               (exist [x y]
                      (cond-e
                       ((teacup-o x) (== true y) s*)
                       ((== false x) (== true y)))
                      (== (cons x (cons y ())) r)))
         '[(false true) (tea true) (cup true)])))

(deftest test-cons-o
  (is (= (run* [q]
               (exist [a d]
                      (cons-o a d '())
                      (== (cons a d) q))
        []))))

(deftest test-cons-o-2
  (is (= (run* [q]
               (== [q] nil))
         [])))

(deftest test-cons-o-3
  (is (=
       (run* [q]
             (cons-o 'a nil q))
       '[(a)])))

(deftest test-cons-o-4
  (is (= (run* [q]
               (cons-o 'a '(d) q))
         '[(a d)])))

(deftest test-cons-o-empty-list
  (is (= (run* [q]
               (cons-o 'a q '(a)))
         '[()])))

(deftest test-cons-o-5
  (is (= (run* [q]
               (cons-o q '(b c) '(a b c)))
         '[a])))

(deftest test-first-o
  (is (= (run* [q]
               (first-o q '(1 2)))
         '[1])))

(deftest test-rest-o
  (is (= (run* [q]
               (rest-o q '(1 2)))
         '[(_.0 1 2)])))

(deftest test-rest-o-2
  (is (= (run* [q]
               (rest-o q [1 2]))
         '[(_.0 1 2)])))

(deftest test-rest-o-3
  (is (= (run* [q]
               (rest-o [1 2] q))
         '[(2)])))

(deftest test-rest-o-4
  (is (= (run* [q]
               (rest-o [1 2 3 4 5 6 7 8] q))
         '[(2 3 4 5 6 7 8)])))

(deftest test-flatten-o
  (is (= (run* [x]
               (flatten-o '[[a b] c] x))
         '[([[a b] c])
           ([a b] (c))
           ([a b] c)
           (a (b) (c))
           ([a b] c ())
           (a (b) c)
           (a (b) c ())
           (a b (c))
           (a b () (c))
           (a b c)
           (a b c ())
           (a b () c)
           (a b () c ())])))

(deftest test-cons-o-1
  (let [a (lvar 'a)
        d (lvar 'd)]
    (is (= (run* [q]
                 (cons-o a d q))
           [(lcons a d)]))))

(comment
  ;; time to implement equality
  )