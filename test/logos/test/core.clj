(ns logos.test.core
  (:refer-clojure :exclude [reify == inc])
  (:use [logos.minikanren] :reload)
  (:use [logos.logic] :reload)
  (:use clojure.test clojure.pprint)
  (:require [clojure.contrib.macro-utils :as macro]))

;; =============================================================================
;; unify

;; -----------------------------------------------------------------------------
;; nil

(deftest unify-nil-object-1
  (is (= (unify empty-s nil 1) false)))

(deftest unify-nil-lvar-1
  (let [x (lvar 'x)
        os (ext-no-check empty-s x nil)]
   (is (= (unify empty-s nil x) os))))

(deftest unify-nil-lseq-1
  (let [x (lvar 'x)]
   (is (= (unify empty-s nil (lcons 1 x)) false))))

(deftest unify-nil-map-1
  (let [x (lvar 'x)]
   (is (= (unify empty-s nil {}) false))))

(deftest unify-nil-set-1
  (let [x (lvar 'x)]
   (is (= (unify empty-s nil #{}) false))))

;; -----------------------------------------------------------------------------
;; object

(deftest unify-object-nil-1
  (is (= (unify empty-s 1 nil))))

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
        lc1 (lcons 1 (lcons 2 x))
        lc2 (lcons 1 (lcons 2 y))
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

(deftest unify-seq-lcons-1
  (let [x (lvar 'x)
        lc1 (lcons 1 (lcons 2 x))
        l1 '(1 2 3 4)
        os (ext-no-check empty-s x '(3 4))]
    (is (= (unify empty-s l1 lc1) os))))

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

(deftest unify-seq-seq-11
  (is (= (unify empty-s [[1 2]] [[1 2]]) empty-s)))

(deftest unify-seq-seq-12
  (is (= (unify empty-s [[1 2]] [[2 1]]) false)))

(deftest unify-seq-seq-13
  (let [x (lvar 'x)
        os (ext-no-check empty-s x 1)]
   (is (= (unify empty-s [[x 2]] [[1 2]]) os))))

(deftest unify-seq-seq-14
  (let [x (lvar 'x)
        os (ext-no-check empty-s x [1 2])]
   (is (= (unify empty-s [x] [[1 2]]) os))))

(deftest unify-seq-seq-15
  (let [x (lvar 'x) y (lvar 'y)
        u (lvar 'u) v (lvar 'v)
        os (-> empty-s
               (ext-no-check x 'b)
               (ext-no-check y 'a))]
   (is (= (unify empty-s ['a x] [y 'b]) os))))

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

(deftest unify-map-map-1
  (is (= (unify empty-s {} {}) empty-s)))

(deftest unify-map-map-2
  (is (= (unify empty-s {1 2 3 4} {1 2 3 4}) empty-s)))

(deftest unify-map-map-3
  (is (= (unify empty-s {1 2} {1 2 3 4}) false)))

(deftest unify-map-map-4
  (let [x (lvar 'x)
        m1 {1 2 3 4}
        m2 {1 2 3 x}
        os (ext-no-check empty-s x 4)]
   (is (= (unify empty-s m1 m2) os))))

(deftest unify-map-map-5
  (let [x (lvar 'x)
        m1 {1 2 3 4}
        m2 {1 4 3 x}]
   (is (= (unify empty-s m1 m2) false))))

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

(deftest unify-set-set-1
  (is (= (unify empty-s #{} #{}) empty-s)))

(deftest unify-set-set-2
  (is (= (unify empty-s #{} #{1}) false)))

(deftest unify-set-set-3
  (let [x (lvar 'x)
        os (ext-no-check empty-s x 1)]
   (is (= (unify empty-s #{x} #{1}) os))))

(deftest unify-set-set-4
  (let [x (lvar 'x)
        y (lvar 'y)
        os (-> empty-s
               (ext-no-check x 2)
               (ext-no-check y 1))]
   (is (= (unify empty-s #{1 x} #{2 y}) os))))

(deftest unify-set-set-5
  (let [x (lvar 'x)
        y (lvar 'y)
        os (-> empty-s
               (ext-no-check x 2)
               (ext-no-check y 1))]
   (is (= (unify empty-s #{x 1} #{2 y}) os))))

(deftest unify-set-set-6
  (let [a (lvar 'a)
        b (lvar 'b)
        c (lvar 'c)
        d (lvar 'd)
        s (.s (unify empty-s #{a b 3 4 5} #{1 2 3 c d}))]
    (is (and (= (count s) 4)
             (= (set (keys s)) #{a b c d})
             (= (set (vals s)) #{1 2 4 5})))))

(deftest unify-set-set-7
  (let [a (lvar 'a)
        b (lvar 'b)
        c (lvar 'c)
        d (lvar 'd)]
    (is (= (unify empty-s #{a b 9 4 5} #{1 2 3 c d}) false))))

;; =============================================================================
;; walk

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

;; =============================================================================
;; reify

(deftest test-reify-lvar-name
  (is (= (let [x  (lvar 'x)
               y  (lvar 'y)]
           (reify-lvar-name (to-s [[x 5] [y x]])))
         '_.2)))

;; =============================================================================
;; walk*

(deftest test-walk*
  (is (= (let [x  (lvar 'x)
               y  (lvar 'y)]
           (walk* (to-s [[x 5] [y x]]) `(~x ~y)))
         '(5 5))))

;; =============================================================================
;; run and unify

(deftest test-basic-unify
  (is (= (run* [q]
               (== true q))
         '(true))))

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

;; =============================================================================
;; fail

(deftest test-basic-failure
  (is (= (run* [q]
               fail
               (== true q))
         [])))

;; =============================================================================
;; succeed

;; =============================================================================
;; cond-e

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

(defn teacup-o [x]
  (cond-e
   ((== 'tea x) s#)
   ((== 'cup x) s#)))

(deftest test-basic-conde-e-3
  (is (= (run* [r]
               (exist [x y]
                      (cond-e
                       ((teacup-o x) (== true y) s#)
                       ((== false x) (== true y)))
                      (== (cons x (cons y ())) r)))
         '((false true) (tea true) (cup true)))))

;; =============================================================================
;; cons-o

(deftest test-cons-o
  (is (= (run* [q]
               (exist [a d]
                      (cons-o a d '())
                      (== (cons a d) q))
               []))))

(deftest test-cons-o-1
  (let [a (lvar 'a)
        d (lvar 'd)]
    (is (= (run* [q]
                 (cons-o a d q))
           [(lcons a d)]))))

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

;; =============================================================================
;; first-o

(deftest test-first-o
  (is (= (run* [q]
               (first-o q '(1 2)))
         (list (lcons '(1 2) (lvar 'x))))))

;; =============================================================================
;; rest-o

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

;; =============================================================================
;; flatten-o

(deftest test-flatten-o
  (is (= (run* [x]
               (flatten-o '[[a b] c] x))
         '(([[a b] c]) ([a b] (c)) ([a b] c) (a (b) (c)) ([a b] c ()) (a (b) c) (a (b) c ()) (a b (c)) (a b c) (a b () (c)) (a b c ()) (a b () c) (a b () c ())))))

;; =============================================================================
;; member-o

(deftest member-o-1
  (is (= (run* [q]
               (macro/symbol-macrolet
                [_ (lvar)]
                (all
                 (== q [_])
                 (member-o ['foo _] q)
                 (member-o [_ 'bar] q))))
         '([[foo bar]]))))

(deftest member-o-2
  (is (= (run* [q]
               (macro/symbol-macrolet
                [_ (lvar)]
                (all
                 (== q [_ _])
                 (member-o ['foo _] q)
                 (member-o [_ 'bar] q))))
         '([[foo bar] _.0] [[foo _.0] [_.1 bar]] [[_.0 bar] [foo _.1]] [_.0 [foo bar]]))))

;; -----------------------------------------------------------------------------
;; rember-o

(deftest rember-o-1
  (is (= (run 1 [q]
              (rember-o 'b '(a b c b d) q))
         '((a c b d)))))

;; -----------------------------------------------------------------------------
;; cond-e clause count

(defn digit-1 [x]
  (cond-e
   ((== 0 x))))

(defn digit-4 [x]
  (cond-e
   ((== 0 x))
   ((== 1 x))
   ((== 2 x))
   ((== 3 x))))

(deftest test-cond-e-1-clause
  (is (= (run* [q]
               (exist [x y]
                      (digit-1 x)
                      (digit-1 y)
                      (== q [x y])))
         '([0 0]))))

(deftest test-cond-e-4-clauses
  (is (= (run* [q]
               (exist [x y]
                      (digit-4 x)
                      (digit-4 y)
                      (== q [x y])))
         '([0 0] [0 1] [0 2] [1 0] [0 3] [1 1] [1 2] [2 0] [1 3] [2 1] [3 0] [2 2] [3 1] [2 3] [3 2] [3 3]))))

;; -----------------------------------------------------------------------------
;; any-o

(defn any-o [q]
  (cond-e
   (q s#)
   ((any-o q))))

(deftest test-any-o-1
  (is (= (run 1 [q]
              (any-o s#)
              (== true q))
         (list true))))

(deftest test-any-o-2
  (is (= (run 5 [q]
              (any-o s#)
              (== true q))
         (list true true true true true))))

;; -----------------------------------------------------------------------------
;; divergence

(def f1 (exist [] f1))

(deftest test-divergence-1
  (is (= (run 1 [q]
            (cond-e
             (f1)
             ((== false false))))
         '(_.0))))

(deftest test-divergence-2
  (is (= (run 1 [q]
            (cond-e
             (f1 (== false false))
             ((== false false))))
         '(_.0))))

(def f2
     (exist []
            (cond-e
             (f2 (cond-e
                  (f2) 
                  ((== false false))))
             ((== false false)))))

(deftest test-divergence-3
  (is (= (run 5 [q] f2)
         '(_.0 _.0 _.0 _.0 _.0))))