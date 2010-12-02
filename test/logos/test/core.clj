(ns logos.test.core
  (:use [logos.minikanren] :reload)
  (:use [clojure.test]))

(defn teacup-o [x]
  (cond-e
   ((== 'tea x) s#)
   ((== 'cup x) s#)))

(deftest test-basic-lookup
  (is (= (let [x  (lvar 'x)
               y  (lvar 'y)
               ss (to-s [[x 5] [y x]])]
           (lookup ss y))
         5)))

(deftest test-deep-lookup
  (is (= (let [[x y z c b a :as s] (map lvar '[x y z c b a])
               ss (to-s [[x 5] [y x] [z y] [c z] [b c] [a b]])]
           (lookup ss a)))
      5))

(deftest test-reify-lvar-name
  (is (= (let [x  (lvar 'x)
               y  (lvar 'y)]
           (reify-lvar-name (to-s [[x 5] [y x]])))
         '_.2)))

(deftest test-reify-lookup
  (is (= (let [x  (lvar 'x)
               y  (lvar 'y)]
           (reify-lookup (to-s [[x 5] [y x]]) `(~x ~y)))
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
                       ((teacup-o x) (== true y) s#)
                       ((== false x) (== true y)))
                      (== (cons x (cons y ())) r)))
         '[(false true) (tea true) (cup true)])))