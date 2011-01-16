(ns logos.test.core
  (:refer-clojure :exclude [reify ==])
  (:use [logos.minikanren] :reload)
  (:use [logos.logic] :reload)
  (:use [clojure.test]))

;; =============================================================================
;; unify

;; -----------------------------------------------------------------------------
;; object

(deftest unify-object-object-1
  (is (= (unify empty-s 1 1) empty-s)))

(deftest unify-object-object-2
  (is (= (unify empty-s :foo :foo) empty-s)))

(deftest unify-object-object-3
  (is (= (unify empty-s 'foo 'foo) empty-s)))

(deftest unify-object-object-4
  (is (= (unify empty-s "foo" "foo") empty-s)))

(deftest unify-object-object-5
  (is (= (unify empty-s 1 2) false)))

(deftest unify-object-object-6
  (is (= (unify empty-s 2 1) false)))

(deftest unify-object-object-7
  (is (= (unify empty-s :foo :bar) false)))

(deftest unify-object-object-8
  (is (= (unify empty-s 'foo 'bar) false)))

(deftest unify-object-object-9
  (is (= (unify empty-s "foo" "bar") false)))

(deftest unify-object-lvar-1
  (let [x (lvar 'x)
        os (ext-no-check empty-s x 1)]
    (is (= (unify empty-s 1 x) os))))

(deftest unify-object-lcons-1
  (let [x (lvar 'x)]
   (is (= (unify empty-s 1 (lcons 1 'x)) false))))

(deftest unify-object-seq-1
  (is (= (unify empty-s 1 '()) false)))

(deftest unify-object-seq-2
  (is (= (unify empty-s 1 '[]) false)))

(deftest unify-object-map-1
  (is (= (unify empty-s 1 {}) false)))

(deftest unify-object-set-1
  (is (= (unify empty-s 1 #{}) false)))

;; -----------------------------------------------------------------------------
;; lvar

(deftest unify-lvar-object-1
  (let [x (lvar 'x)
        os (ext-no-check empty-s x 1)]
    (is (= (unify empty-s x 1) os))))

(deftest unify-lvar-lvar-1
  (let [x (lvar 'x)
        y (lvar 'y)
        os (ext-no-check empty-s x y)]
    (is (= (unify empty-s x y) os))))

(deftest unify-lvar-lcons-1
  (let [x (lvar 'x)
        y (lvar 'y)
        l (lcons 1 y)
        os (ext-no-check empty-s x l)]
    (is (= (unify empty-s x l) os))))

(deftest unify-lvar-seq-1
  (let [x (lvar 'x)
        os (ext-no-check empty-s x [])]
    (is (= (unify empty-s x []) os))))

(deftest unify-lvar-seq-2
  (let [x (lvar 'x)
        os (ext-no-check empty-s x [1 2 3])]
    (is (= (unify empty-s x [1 2 3]) os))))

(deftest unify-lvar-seq-3
  (let [x (lvar 'x)
        os (ext-no-check empty-s x '())]
    (is (= (unify empty-s x '()) os))))

(deftest unify-lvar-seq-4
  (let [x (lvar 'x)
        os (ext-no-check empty-s x '(1 2 3))]
    (is (= (unify empty-s x '(1 2 3)) os))))

(deftest unify-lvar-map-1
  (let [x (lvar 'x)
        os (ext-no-check empty-s x {})]
    (is (= (unify empty-s x {}) os))))

(deftest unify-lvar-map-2
  (let [x (lvar 'x)
        os (ext-no-check empty-s x {1 2 3 4})]
    (is (= (unify empty-s x {1 2 3 4}) os))))

(deftest unify-lvar-set-1
  (let [x (lvar 'x)
        os (ext-no-check empty-s x #{})]
    (is (= (unify empty-s x #{}) os))))

(deftest unify-lvar-set-2
  (let [x (lvar 'x)
        os (ext-no-check empty-s x #{1 2 3})]
    (is (= (unify empty-s x #{1 2 3}) os))))

;; -----------------------------------------------------------------------------
;; lcons

(deftest unify-lcons-object-1
  (let [x (lvar 'x)]
    (is (= (unify empty-s (lcons 1 x) 1) false))))

(deftest unify-lcons-lvar-1
  (let [x (lvar 'x)
        y (lvar 'y)
        l (lcons 1 y)
        os (ext-no-check empty-s x l)]
    (is (= (unify empty-s l x) os))))

(deftest unify-lcons-lcons-1
  (let [x (lvar 'x)
        y (lvar 'y)
        lc1 (lcons 1 x)
        lc2 (lcons 1 y)
        os (ext-no-check empty-s x y)]
    (is (= (unify empty-s lc1 lc2) os))))

(deftest unify-lcons-lcons-2
  (let [x (lvar 'x)
        y (lvar 'y)
        z (lvar 'z)
        lc1 (lcons 1 (lcons 2 x))
        lc2 (lcons 1 (lcons z y))
        os (-> empty-s
            (ext-no-check x y)
            (ext-no-check z 2))]
    (is (= (unify empty-s lc1 lc2) os))))

(deftest unify-lcons-lcons-3
  (let [x (lvar 'x)
        y (lvar 'y)
        lc1 (lcons 1 (lcons 2 x))
        lc2 (lcons 1 (lcons 2 (lcons 3 y)))
        os (ext-no-check empty-s x (lcons 3 y))]
    (is (= (unify empty-s lc1 lc2) os))))

(deftest unify-lcons-lcons-4
  (let [x (lvar 'x)
        y (lvar 'y)
        lc1 (lcons 1 (lcons 2 x))
        lc2 (lcons 1 (lcons 3 (lcons 4 y)))]
    (is (= (unify empty-s lc1 lc2) false))))

(deftest unify-lcons-lcons-5
  (let [x (lvar 'x)
        y (lvar 'y)
        lc2 (lcons 1 (lcons 2 x))
        lc1 (lcons 1 (lcons 3 (lcons 4 y)))]
    (is (= (unify empty-s lc1 lc2) false))))

(deftest unify-lcons-lcons-6
  (let [x (lvar 'x)
        y (lvar 'y)
        lc2 (lcons 1 (lcons 2 x))
        lc1 (lcons 1 (lcons 2 y))
        os (ext-no-check empty-s x y)]
    (is (= (unify empty-s lc1 lc2) os))))

(deftest unify-lcons-seq-1
  (let [x (lvar 'x)
        lc1 (lcons 1 (lcons 2 x))
        l1 '(1 2 3 4)
        os (ext-no-check empty-s x '(3 4))]
    (is (= (unify empty-s lc1 l1) os))))

(deftest unify-lcons-seq-2
  (let [x (lvar 'x)
        y (lvar 'y)
        lc1 (lcons 1 (lcons y (lcons 3 x)))
        l1 '(1 2 3 4)
        os (-> empty-s
               (ext-no-check x '(4))
               (ext-no-check y 2))]
    (is (= (unify empty-s lc1 l1) os))))

(deftest unify-lcons-seq-3
  (let [x (lvar 'x)
        lc1 (lcons 1 (lcons 2 (lcons 3 x)))
        l1 '(1 2 3)
        os (ext-no-check empty-s x '())]
    (is (= (unify empty-s lc1 l1) os))))

(deftest unify-lcons-seq-4
  (let [x (lvar 'x)
        lc1 (lcons 1 (lcons 3 x))
        l1 '(1 2 3 4)]
    (is (= (unify empty-s lc1 l1) false))))

(deftest unify-lcons-seq-5
  (let [x (lvar 'x)
        lc1 (lcons 1 (lcons 2 x))
        l1 '(1 3 4 5)]
    (is (= (unify empty-s lc1 l1) false))))

(deftest unify-lcons-map-1
  (is (= (unify empty-s (lcons 1 (lvar 'x)) {}) false)))

(deftest unify-lcons-set-1
  (is (= (unify empty-s (lcons 1 (lvar 'x)) #{}) false)))

;; -----------------------------------------------------------------------------
;; seq

(deftest unify-seq-object-1
  (is (= (unify empty-s '() 1) false)))

(deftest unify-seq-object-2
  (is (= (unify empty-s [] 1) false)))

(deftest unify-seq-lvar-1
  (let [x (lvar 'x)
        os (ext-no-check empty-s x [])]
    (is (= (unify empty-s [] x) os))))

;; unify-seq-lcons-1, same as unify-lcons-seq

(deftest unify-seq-seq-1
  (is (= (unify empty-s [1 2 3] [1 2 3]) empty-s)))

(deftest unify-seq-seq-2
  (is (= (unify empty-s '(1 2 3) [1 2 3]) empty-s)))

(deftest unify-seq-seq-3
  (is (= (unify empty-s '(1 2 3) '(1 2 3)) empty-s)))

(deftest unify-seq-seq-4
  (let [x (lvar 'x)
        os (ext-no-check empty-s x 2)]
    (is (= (unify empty-s `(1 ~x 3) `(1 2 3)) os))))

(deftest unify-seq-seq-5
  (is (= (unify empty-s [1 2] [1 2 3]) false)))

(deftest unify-seq-seq-6
  (is (= (unify empty-s '(1 2) [1 2 3]) false)))

(deftest unify-seq-seq-7
  (is (= (unify empty-s [1 2 3] [3 2 1]) false)))

(deftest unify-seq-seq-8
  (is (= (unify empty-s '() '()) empty-s)))

(deftest unify-seq-seq-9
  (is (= (unify empty-s '() '(1)) false)))

(deftest unify-seq-seq-10
  (is (= (unify empty-s '(1) '()) false)))

(deftest unify-seq-map-1
  (is (= (unify empty-s [] {}) false)))

(deftest unify-seq-map-2
  (is (= (unify empty-s '() {}) false)))

(deftest unify-seq-set-1
  (is (= (unify empty-s [] #{}) false)))

(deftest unify-seq-set-2
  (is (= (unify empty-s '() #{}) false)))

;; -----------------------------------------------------------------------------
;; map

(deftest unify-map-object-1
  (is (= (unify empty-s {} 1) false)))

(deftest unify-map-lvar-1
  (let [x (lvar 'x)
        os (ext-no-check empty-s x {})]
    (is (= (unify empty-s {} x) os))))

(deftest unify-map-lcons-1
  (let [x (lvar 'x)]
   (is (= (unify empty-s {} (lcons 1 x)) false))))

(deftest unify-map-seq-1
  (is (= (unify empty-s {} '()) false)))

;; unify-map-map-1

;; unify-map-map-2, lvar in key

;; unify-map-map-3, lvar in val

;; unify-map-map-4, lvar in key and val

(deftest unify-map-set-1
  (is (= (unify empty-s {} #{}) false)))

;; -----------------------------------------------------------------------------
;; set

(deftest unify-set-object-1
  (is (= (unify empty-s #{} 1) false)))

(deftest unify-set-lvar-1
  (let [x (lvar 'x)
        os (ext-no-check empty-s x #{})]
    (is (= (unify empty-s #{} x) os))))

(deftest unify-set-lcons-1
  (let [x (lvar 'x)]
   (is (= (unify empty-s #{} (lcons 1 x)) false))))

(deftest unify-set-seq-1
  (is (= (unify empty-s #{} '()) false)))

(deftest unify-set-map-1
  (is (= (unify empty-s #{} {}) false)))

;; unify-set-set-1

;; unify-set-set-2, with 1 lvar

;; unify-set-set-3, with 2 lvars

;; unify-set-set-4, with 3 lvars

;; =============================================================================
;; walk

;; =============================================================================
;; reify

;; =============================================================================
;; cond-e

;; =============================================================================
;; cons-o

;; =============================================================================
;; rest-o

(comment
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
          '((cup true) (tea true) (false true)))))

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
          (list (lcons '(1 2) (lvar 'x))))))

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
          '((a b c)
            ([[a b] c])
            ([a b] c ())
            (a b c ())
            (a b (c))
            (a b () c ())
            ([a b] (c))
            ([a b] c)
            (a (b) c ())
            (a b () (c))
            (a b () c)
            (a (b) (c))
            (a (b) c)))))

 (deftest test-cons-o-1
   (let [a (lvar 'a)
         d (lvar 'd)]
     (is (= (run* [q]
                  (cons-o a d q))
            [(lcons a d)]))))
 )

(comment
  ;; time to implement equality
  )