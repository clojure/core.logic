(ns clojure.core.logic.tests
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        clojure.test :reload)
  (:require [clojure.pprint :as pp]))

;; =============================================================================
;; unify

;; -----------------------------------------------------------------------------
;; nil

(deftest unify-nil-object-1
  (is (= (unify empty-s nil 1) nil)))

(deftest unify-nil-lvar-1
  (let [x (lvar 'x)
        os (ext-no-check empty-s x nil)]
    (is (= (unify empty-s nil x) os))))

(deftest unify-nil-lseq-1
  (let [x (lvar 'x)]
    (is (= (unify empty-s nil (lcons 1 x)) nil))))

(deftest unify-nil-map-1
  (let [x (lvar 'x)]
    (is (= (unify empty-s nil {}) nil))))

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
  (is (= (unify empty-s 1 2) nil)))

(deftest unify-object-object-6
  (is (= (unify empty-s 2 1) nil)))

(deftest unify-object-object-7
  (is (= (unify empty-s :foo :bar) nil)))

(deftest unify-object-object-8
  (is (= (unify empty-s 'foo 'bar) nil)))

(deftest unify-object-object-9
  (is (= (unify empty-s "foo" "bar") nil)))

(deftest unify-object-lvar-1
  (let [x (lvar 'x)
        os (ext-no-check empty-s x 1)]
    (is (= (unify empty-s 1 x) os))))

(deftest unify-object-lcons-1
  (let [x (lvar 'x)]
    (is (= (unify empty-s 1 (lcons 1 'x)) nil))))

(deftest unify-object-seq-1
  (is (= (unify empty-s 1 '()) nil)))

(deftest unify-object-seq-2
  (is (= (unify empty-s 1 '[]) nil)))

(deftest unify-object-map-1
  (is (= (unify empty-s 1 {}) nil)))

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

;; -----------------------------------------------------------------------------
;; lcons

(deftest unify-lcons-object-1
  (let [x (lvar 'x)]
    (is (= (unify empty-s (lcons 1 x) 1) nil))))

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
    (is (= (unify empty-s lc1 lc2) nil))))

(deftest unify-lcons-lcons-5
  (let [x (lvar 'x)
        y (lvar 'y)
        lc2 (lcons 1 (lcons 2 x))
        lc1 (lcons 1 (lcons 3 (lcons 4 y)))]
    (is (= (unify empty-s lc1 lc2) nil))))

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
    (is (= (unify empty-s lc1 l1) nil))))

(deftest unify-lcons-seq-5
  (let [x (lvar 'x)
        lc1 (lcons 1 (lcons 2 x))
        l1 '(1 3 4 5)]
    (is (= (unify empty-s lc1 l1) nil))))

(deftest unify-lcons-map-1
  (is (= (unify empty-s (lcons 1 (lvar 'x)) {}) nil)))

;; -----------------------------------------------------------------------------
;; seq

(deftest unify-seq-object-1
  (is (= (unify empty-s '() 1) nil)))

(deftest unify-seq-object-2
  (is (= (unify empty-s [] 1) nil)))

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
  (is (= (unify empty-s [1 2] [1 2 3]) nil)))

(deftest unify-seq-seq-6
  (is (= (unify empty-s '(1 2) [1 2 3]) nil)))

(deftest unify-seq-seq-7
  (is (= (unify empty-s [1 2 3] [3 2 1]) nil)))

(deftest unify-seq-seq-8
  (is (= (unify empty-s '() '()) empty-s)))

(deftest unify-seq-seq-9
  (is (= (unify empty-s '() '(1)) nil)))

(deftest unify-seq-seq-10
  (is (= (unify empty-s '(1) '()) nil)))

(deftest unify-seq-seq-11
  (is (= (unify empty-s [[1 2]] [[1 2]]) empty-s)))

(deftest unify-seq-seq-12
  (is (= (unify empty-s [[1 2]] [[2 1]]) nil)))

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
  (is (= (unify empty-s [] {}) nil)))

(deftest unify-seq-map-2
  (is (= (unify empty-s '() {}) nil)))

;; -----------------------------------------------------------------------------
;; map

(deftest unify-map-object-1
  (is (= (unify empty-s {} 1) nil)))

(deftest unify-map-lvar-1
  (let [x (lvar 'x)
        os (ext-no-check empty-s x {})]
    (is (= (unify empty-s {} x) os))))

(deftest unify-map-lcons-1
  (let [x (lvar 'x)]
    (is (= (unify empty-s {} (lcons 1 x)) nil))))

(deftest unify-map-seq-1
  (is (= (unify empty-s {} '()) nil)))

(deftest unify-map-map-1
  (is (= (unify empty-s {} {}) empty-s)))

(deftest unify-map-map-2
  (is (= (unify empty-s {1 2 3 4} {1 2 3 4}) empty-s)))

(deftest unify-map-map-3
  (is (= (unify empty-s {1 2} {1 2 3 4}) nil)))

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
    (is (= (unify empty-s m1 m2) nil))))

(defstruct foo-struct :a :b)

(deftest unify-struct-map-1
  (let [x (lvar 'x)
        m1 (struct-map foo-struct :a 1 :b 2)
        m2 (struct-map foo-struct :a 1 :b x)
        os (ext-no-check empty-s x 2)]
    (is (= (unify empty-s m1 m2) os))))

(deftest unify-struct-map-2
  (let [x (lvar 'x)
        m1 (struct-map foo-struct :a 1 :b 2)
        m2 (struct-map foo-struct :a 1 :b 3)]
    (is (= (unify empty-s m1 m2) nil))))

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
           (fresh [x y]
             (== [x y] [1 5])
             (== [x y] q)))
         [[1 5]])))

(deftest test-basic-unify-3
  (is (=  (run* [q]
            (fresh [x y]
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
;; Basic

(deftest test-all
  (is (= (run* [q]
           (all
            (== 1 1)
            (== q true)))
         '(true))))

;; =============================================================================
;; TRS

(defn pairo [p]
  (fresh [a d]
    (== (lcons a d) p)))

(defn twino [p]
  (fresh [x]
    (conso x x p)))

(defn listo [l]
  (conde
    [(emptyo l) s#]
    [(pairo l)
     (fresh [d]
       (resto l d)
       (listo d))]))

(defn flatteno [s out]
  (conde
    [(emptyo s) (== '() out)]
    [(pairo s)
     (fresh [a d res-a res-d]
       (conso a d s)
       (flatteno a res-a)
       (flatteno d res-d)
       (appendo res-a res-d out))]
    [(conso s '() out)]))

;; =============================================================================
;; conde

(deftest test-basic-conde
  (is (=  (run* [x]
            (conde
              [(== x 'olive) succeed]
              [succeed succeed]
              [(== x 'oil) succeed]))
          '[olive _.0 oil])))

(deftest test-basic-conde-2
  (is (= (run* [r]
           (fresh [x y]
             (conde
               [(== 'split x) (== 'pea y)]
               [(== 'navy x) (== 'bean y)])
             (== (cons x (cons y ())) r)))
         '[(split pea) (navy bean)])))

(defn teacupo [x]
  (conde
    [(== 'tea x) s#]
    [(== 'cup x) s#]))

(deftest test-basic-conde-e-3
  (is (= (run* [r]
           (fresh [x y]
             (conde
               [(teacupo x) (== true y) s#]
               [(== false x) (== true y)])
             (== (cons x (cons y ())) r)))
         '((false true) (tea true) (cup true)))))

;; =============================================================================
;; conso

(deftest test-conso
  (is (= (run* [q]
           (fresh [a d]
             (conso a d '())))
         ())))

(deftest test-conso-1
  (let [a (lvar 'a)
        d (lvar 'd)]
    (is (= (run* [q]
             (conso a d q))
           [(lcons a d)]))))

(deftest test-conso-2
  (is (= (run* [q]
           (== [q] nil))
         [])))

(deftest test-conso-3
  (is (=
       (run* [q]
         (conso 'a nil q))
       '[(a)])))

(deftest test-conso-4
  (is (= (run* [q]
           (conso 'a '(d) q))
         '[(a d)])))

(deftest test-conso-empty-list
  (is (= (run* [q]
           (conso 'a q '(a)))
         '[()])))

(deftest test-conso-5
  (is (= (run* [q]
           (conso q '(b c) '(a b c)))
         '[a])))

;; =============================================================================
;; firsto

(deftest test-firsto
  (is (= (run* [q]
           (firsto q '(1 2)))
         (list (lcons '(1 2) (lvar 'x))))))

;; =============================================================================
;; resto

(deftest test-resto
  (is (= (run* [q]
           (resto q '(1 2)))
         '[(_.0 1 2)])))

(deftest test-resto-2
  (is (= (run* [q]
           (resto q [1 2]))
         '[(_.0 1 2)])))

(deftest test-resto-3
  (is (= (run* [q]
           (resto [1 2] q))
         '[(2)])))

(deftest test-resto-4
  (is (= (run* [q]
           (resto [1 2 3 4 5 6 7 8] q))
         '[(2 3 4 5 6 7 8)])))

;; =============================================================================
;; flatteno

(deftest test-flatteno
  (is (= (run* [x]
           (flatteno '[[a b] c] x))
         '(([[a b] c]) ([a b] (c)) ([a b] c) ([a b] c ())
           (a (b) (c)) (a (b) c) (a (b) c ()) (a b (c))
           (a b () (c)) (a b c) (a b c ()) (a b () c)
           (a b () c ())))))

;; =============================================================================
;; membero

(deftest membero-1
  (is (= (run* [q]
           (all
            (== q [(lvar)])
            (membero ['foo (lvar)] q)
            (membero [(lvar) 'bar] q)))
         '([[foo bar]]))))

(deftest membero-2
  (is (= (run* [q]
           (all
            (== q [(lvar) (lvar)])
            (membero ['foo (lvar)] q)
            (membero [(lvar) 'bar] q)))
         '([[foo bar] _.0] [[foo _.0] [_.1 bar]]
             [[_.0 bar] [foo _.1]] [_.0 [foo bar]]))))

;; -----------------------------------------------------------------------------
;; rembero

(deftest rembero-1
  (is (= (run 1 [q]
           (rembero 'b '(a b c b d) q))
         '((a c b d)))))

;; -----------------------------------------------------------------------------
;; conde clause count

(defn digit-1 [x]
  (conde
    [(== 0 x)]))

(defn digit-4 [x]
  (conde
    [(== 0 x)]
    [(== 1 x)]
    [(== 2 x)]
    [(== 3 x)]))

(deftest test-conde-1-clause
  (is (= (run* [q]
           (fresh [x y]
             (digit-1 x)
             (digit-1 y)
             (== q [x y])))
         '([0 0]))))

(deftest test-conde-4-clauses
  (is (= (run* [q]
           (fresh [x y]
             (digit-4 x)
             (digit-4 y)
             (== q [x y])))
         '([0 0] [0 1] [0 2] [1 0] [0 3] [1 1] [1 2] [2 0]
             [1 3] [2 1] [3 0] [2 2] [3 1] [2 3] [3 2] [3 3]))))

;; -----------------------------------------------------------------------------
;; anyo

(defn anyo [q]
  (conde
    [q s#]
    [(anyo q)]))

(deftest test-anyo-1
  (is (= (run 1 [q]
           (anyo s#)
           (== true q))
         (list true))))

(deftest test-anyo-2
  (is (= (run 5 [q]
           (anyo s#)
           (== true q))
         (list true true true true true))))

;; -----------------------------------------------------------------------------
;; divergence

(def f1 (fresh [] f1))

(deftest test-divergence-1
  (is (= (run 1 [q]
           (conde
             [f1]
             [(== false false)]))
         '(_.0))))

(deftest test-divergence-2
  (is (= (run 1 [q]
           (conde
             [f1 (== false false)]
             [(== false false)]))
         '(_.0))))

(def f2
  (fresh []
    (conde
      [f2 (conde
            [f2] 
            [(== false false)])]
      [(== false false)])))

(deftest test-divergence-3
  (is (= (run 5 [q] f2)
         '(_.0 _.0 _.0 _.0 _.0))))

;; -----------------------------------------------------------------------------
;; conda (soft-cut)

(deftest test-conda-1
  (is (= (run* [x]
           (conda
             [(== 'olive x) s#]
             [(== 'oil x) s#]
             [u#]))
         '(olive))))

(deftest test-conda-2
  (is (= (run* [x]
           (conda
             [(== 'virgin x) u#]
             [(== 'olive x) s#]
             [(== 'oil x) s#]
             [u#]))
         '())))

(deftest test-conda-3
  (is (= (run* [x]
           (fresh (x y)
             (== 'split x)
             (== 'pea y)
             (conda
               [(== 'split x) (== x y)]
               [s#]))
           (== true x))
         '())))

(deftest test-conda-4
  (is (= (run* [x]
           (fresh (x y)
             (== 'split x)
             (== 'pea y)
             (conda
               [(== x y) (== 'split x)]
               [s#]))
           (== true x))
         '(true))))

(defn not-pastao [x]
  (conda
    [(== 'pasta x) u#]
    [s#]))

(deftest test-conda-5
  (is (= (run* [x]
           (conda
             [(not-pastao x)]
             [(== 'spaghetti x)]))
         '(spaghetti))))

;; -----------------------------------------------------------------------------
;; condu (committed-choice)

(comment
  (defn onceo [g]
    (condu
      (g s#)))

 (deftest test-condu-1
   (is (= (run* [x]
            (onceo (teacupo x)))
          '(tea))))
 )

(deftest test-condu-2
  (is (= (run* [r]
           (conde
             [(teacupo r) s#]
             [(== false r) s#]))
         '(false tea cup))))

(deftest test-condu-3
  (is (= (run* [r]
           (conda
             [(teacupo r) s#]
             [(== false r) s#]))
         '(tea cup))))

;; -----------------------------------------------------------------------------
;; disequality

(deftest test-disequality-1
  (is (= (run* [q]
           (fresh [x]
             (!= x 1)
             (== q x)))
         '((_.0 :- (!= _.0 1))))))

(deftest test-disequality-2
  (is (= (run* [q]
           (fresh [x]
             (== q x)
             (!= x 1)))
         '((_.0 :- (!= _.0 1))))))

(deftest test-disequality-3
  (is (= (run* [q]
           (fresh [x]
             (!= x 1)
             (== x 1)
             (== q x)))
         ())))

(deftest test-disequality-4
  (is (= (run* [q]
           (fresh [x]
             (== x 1)
             (!= x 1)
             (== q x)))
         ())))

(deftest test-disequality-5
  (is (= (run* [q]
           (fresh [x y]
             (!= x y)
             (== x 1)
             (== y 1)
             (== q x)))
         ())))

(deftest test-disequality-6
  (is (= (run* [q]
           (fresh [x y]
             (== x 1)
             (== y 1)
             (!= x y)
             (== q x)))
         ())))

(deftest test-disequality-7
  (is (= (run* [q]
           (fresh [x y]
             (== x 1)
             (!= x y)
             (== y 2)
             (== q x)))
         '(1))))

(deftest test-disequality-8
  (is (= (run* [q]
           (fresh [x y]
             (!= [x 2] [y 1])
             (== x 1)
             (== y 3)
             (== q [x y])))
         '([1 3]))))

(deftest test-disequality-9
  (is (= (run* [q]
           (fresh [x y]
             (== x 1)
             (== y 3)
             (!= [x 2] [y 1])
             (== q [x y])))
         '([1 3]))))

(deftest test-disequality-10
  (is (= (run* [q]
           (fresh [x y]
             (!= [x 2] [1 y])
             (== x 1)
             (== y 2)
             (== q [x y])))
         ())))

(deftest test-disequality-11
  (is (= (run* [q]
           (fresh [x y]
             (== x 1)
             (== y 2)
             (!= [x 2] [1 y])
             (== q [x y])))
         ())))

(deftest test-disequality-12
  (is (= (run* [q]
           (fresh [x y z]
             (!= x y)
             (== y z)
             (== x z)
             (== q x)))
         ())))

(deftest test-disequality-13
  (is (= (run* [q]
           (fresh [x y z]
             (== y z)
             (== x z)
             (!= x y)
             (== q x)))
         ())))

(deftest test-disequality-14
  (is (= (run* [q]
           (fresh [x y z]
             (== z y)
             (== x z)
             (!= x y)
             (== q x)))
         ())))

(deftest test-disequality-15
  (is (= (run* [q]
           (fresh [x y]
             (== q [x y])
             (!= x 1)
             (!= y 2)))
         '(([_.0 _.1] :- (!= _.1 2) (!= _.0 1))))))

;; -----------------------------------------------------------------------------
;; tabled

(defne arco [x y]
  ([:a :b])
  ([:b :a])
  ([:b :d]))

(def patho
  (tabled [x y]
    (conde
      [(arco x y)]
      [(fresh [z]
         (arco x z)
         (patho z y))])))

(deftest test-tabled-1
  (is (= (run* [q] (patho :a q))
         '(:b :a :d))))

(defne arco-2 [x y]
  ([1 2])
  ([1 4])
  ([1 3])
  ([2 3])
  ([2 5])
  ([3 4])
  ([3 5])
  ([4 5]))

(def patho-2
  (tabled [x y]
    (conde
      [(arco-2 x y)]
      [(fresh [z]
         (arco-2 x z)
         (patho-2 z y))])))

(deftest test-tabled-2
  (let [r (set (run* [q] (patho-2 1 q)))]
    (is (and (= (count r) 4)
             (= r #{2 3 4 5})))))

;; -----------------------------------------------------------------------------
;; rel

(defrel man p)

(fact man 'Bob)
(fact man 'John)
(fact man 'Ricky)

(defrel woman p)
(fact woman 'Mary)
(fact woman 'Martha)
(fact woman 'Lucy)

(defrel likes p1 p2)
(fact likes 'Bob 'Mary)
(fact likes 'John 'Martha)
(fact likes 'Ricky 'Lucy)

(defrel fun p)
(fact fun 'Lucy)

(deftest test-rel-1
  (is (= (run* [q]
           (fresh [x y]
             (likes x y)
             (fun y)
             (== q [x y])))
         '([Ricky Lucy]))))

(retraction likes 'Bob 'Mary)

(deftest test-rel-retract
  (is (= (run* [q]
           (fresh [x y]
             (likes x y)
             (== q [x y])))
         '([John Martha] [Ricky Lucy]))))

(defrel rel1 ^:index a)
(fact rel1 [1 2])

(deftest test-rel-logic-29
  (is (= (run* [q]
           (fresh [a]
             (rel1 [q a])
             (== a 2)))
         '(1))))

(defrel rel2 ^:index e ^:index a ^:index v)
(facts rel2 [[:e1 :a1 :v1]
             [:e1 :a2 :v2]])
(retractions rel2 [[:e1 :a1 :v1]
                   [:e1 :a1 :v1]
                   [:e1 :a2 :v2]])

(deftest rel2-dup-retractions
  (is (= (run* [out]
               (fresh [e a v]
                      (rel2 e :a1 :v1)
                      (rel2 e a v)
                      (== [e a v] out))))
      '()))


;; -----------------------------------------------------------------------------
;; nil in collection

(deftest test-nil-in-coll-1
  (is (= (run* [q]
           (== q [nil]))
         '([nil]))))

(deftest test-nil-in-coll-2
  (is (= (run* [q]
           (== q [1 nil]))
         '([1 nil]))))

(deftest test-nil-in-coll-3
  (is (= (run* [q]
           (== q [nil 1]))
         '([nil 1]))))

(deftest test-nil-in-coll-4
  (is (= (run* [q]
           (== q '(nil)))
         '((nil)))))

(deftest test-nil-in-coll-5
  (is (= (run* [q]
           (== q {:foo nil}))
         '({:foo nil}))))

(deftest test-nil-in-coll-6
  (is (= (run* [q]
           (== q {nil :foo}))
         '({nil :foo}))))

;; -----------------------------------------------------------------------------
;; Unifier

(deftest test-unifier-1
  (is (= (unifier '(?x ?y) '(1 2))
         '(1 2))))

(deftest test-unifier-2
  (is (= (unifier '(?x ?y 3) '(1 2 ?z))
         '(1 2 3))))

(deftest test-unifier-3
  (is (= (unifier '[(?x . ?y) 3] [[1 2] 3])
         '[(1 2) 3])))

(deftest test-unifier-4
  (is (= (unifier '(?x . ?y) '(1 . ?z))
         (lcons 1 '_.0))))

(deftest test-unifier-5
  (is (= (unifier '(?x 2 . ?y) '(1 2 3 4 5))
         '(1 2 3 4 5))))

(deftest test-unifier-6
  (is (= (unifier '(?x 2 . ?y) '(1 9 3 4 5))
         nil)))

(deftest test-unifier-7
  (is (= (unifier '(?x 2 . ?y) '(1 9 3 4 5))
         nil)))

(deftest test-unifier-8 ;;nested maps
  (is (= (unifier '{:a {:b ?b}} {:a {:b 1}})
         {:a {:b 1}})))

(deftest test-unifier-9 ;;nested vectors
  (is (= (unifier '[?a [?b ?c] :d] [:a [:b :c] :d])
         [:a [:b :c] :d])))

(deftest test-unifier-10 ;;nested seqs
  (is (= (unifier '(?a (?b ?c) :d) '(:a (:b :c) :d))
         '(:a (:b :c) :d))))

(deftest test-unifier-11 ;;all together now
  (is (= (unifier '{:a [?b (?c [?d {:e ?e}])]} {:a [:b '(:c [:d {:e :e}])]})
         {:a [:b '(:c [:d {:e :e}])]})))


(deftest test-binding-map-1
  (is (= (binding-map '(?x ?y) '(1 2))
         '{?x 1 ?y 2})))

(deftest test-binding-map-2
  (is (= (binding-map '(?x ?y 3) '(1 2 ?z))
         '{?x 1 ?y 2 ?z 3})))

(deftest test-binding-map-3
  (is (= (binding-map '[(?x . ?y) 3] [[1 2] 3])
         '{?x 1 ?y (2)})))

(deftest test-binding-map-4
  (is (= (binding-map '(?x . ?y) '(1 . ?z))
         '{?z _.0, ?x 1, ?y _.0})))

(deftest test-binding-map-5
  (is (= (binding-map '(?x 2 . ?y) '(1 2 3 4 5))
         '{?x 1 ?y (3 4 5)})))

(deftest test-binding-map-6
  (is (= (binding-map '(?x 2 . ?y) '(1 9 3 4 5))
         nil)))

;; -----------------------------------------------------------------------------
;; Occurs Check

(deftest test-occurs-check-1
  (is (= (run* [q]
           (== q [q]))
         ())))

;; -----------------------------------------------------------------------------
;; Unifications that should fail

(deftest test-unify-fail-1
  (is (= (run* [p] (fresh [a b] (== b ()) (== '(0 1) (lcons a b)) (== p [a b])))
         ())))

(deftest test-unify-fail-2
  (is (= (run* [p] (fresh [a b] (== b '(1)) (== '(0) (lcons a b)) (== p [a b])))
         ())))

(deftest test-unify-fail-3
  (is (= (run* [p] (fresh [a b c d] (== () b) (== '(1) d) (== (lcons a b) (lcons c d)) (== p [a b c d])))
         ())))

;; -----------------------------------------------------------------------------
;; Pattern matching functions preserve metadata

(defne ^:tabled dummy 
  "Docstring"
  [x l]
  ([_ [x . tail]])
  ([_ [head . tail]]
     (membero x tail)))

(deftest test-metadata-defne
  (is (= (-> #'dummy meta :tabled)
         true))
  (is (= (-> #'dummy meta :doc)
         "Docstring")))

(defn locals-membero [x l]
  (matche [l]
          ([[x . tail]])
          ([[head . tail]]
             (locals-membero x tail))))

(deftest test-matche-with-locals
  (is (= [true] (run* [q]
                      (locals-membero 'foo  [1 2 3 4 5 'foo])
                      (== q true))))
  (is (= [] (run* [q]
                  (locals-membero 'foo  [1 2 3 4 5])
                  (== true q)))))

;; -----------------------------------------------------------------------------
;; Pattern matching inline expression support

(defn s [n] (llist n []))

(def zero 0)
(def one (s zero))
(def two (s one))
(def three (s two))
(def four (s three))
(def five (s four))
(def six  (s five))

(defn natural-number [x]
  (matche [x]
    ([zero])
    ([(s y)] (natural-number y))))

(deftest test-matche-with-expr
  (is (= (run* [q] (natural-number one))
         '(_.0 _.0))))

;; -----------------------------------------------------------------------------
;; Pattern matching other data structures

(defne match-map [m o]
  ([{:foo {:bar o}} _]))

(defn test-defne-map []
  (is (= (run* [q]
           (match-map {:foo {:bar 1}} q))
         '(1))))

;; -----------------------------------------------------------------------------
;; Tickets

(deftest test-31-unifier-associative
  (is (= (binding [*reify-vars* false]
           (unifier '{:a ?x} '{:a ?y} '{:a 5}))
         {:a 5}))
  (is (= (binding [*reify-vars* false]
           (unifier '{:a ?x} '{:a 5} '{:a ?y}))
         {:a 5})))

(deftest test-34-unify-with-metadata
  (is (run* [q]
            (== q (quote ^:haz-meta-daytuhs (form form form))))
      '((^:haz-meta-daytuhs (form form form)))))

(deftest test-42-multiple-run-parameters
  (is (= '[[3 _.0 [3 _.0]]]
         (run* [x y z]
           (== z [x y])
           (== [x] [3])))))

(deftest test-49-partial-map-unification
  (is (= '[#clojure.core.logic.PMap{:a 1}]
         (run* [q]
           (fresh [pm x]
             (== pm (partial-map {:a x}))
             (== pm {:a 1 :b 2})
             (== pm q)))))
  (is (= '[#clojure.core.logic.PMap{:a 1}]
         (run* [q]
           (fresh [pm x]
             (== (partial-map {:a x}) pm)
             (== {:a 1 :b 2} pm)
             (== q pm))))))

;; =============================================================================
;; cKanren

(deftest test-pair []
  (is (= (pair 1 2)
         (pair 1 2))))

(deftest test-domfd-1 []
  (let [x (lvar 'x)
        s ((domfd x 1) empty-s)]
    (is (= (:s s) {x 1}))))

#_(deftest test-domfd-2 []
  (let [x (lvar 'x)
        s ((domfd x (interval 1 10)) empty-s)]
    (is (= (:ws s) {x (interval 1 10)}))))

#_(deftest test-domfd-3 []
  (let [x (lvar 'x)
        s ((composeg
            (domfd x (interval 1 10))
            (domfd x (interval 3 6))) empty-s)]
    (is (= (:ws s) {x (interval 3 6)}))))

#_(deftest test-domfd-4 []
  (let [x (lvar 'x)
        s ((composeg
            (domfd x (interval 1 5))
            (domfd x (interval 5 9))) empty-s)]
    (is (= (:s s) {x 5}))
    (is (= (:ws s) {}))))

(deftest test-keep-before-1 []
  (is (= (keep-before (interval 1 10) 5)
         (interval 1 4)))
  (is (= (keep-before (interval 5 10) 5)
         nil))
  (is (= (keep-before (interval 5 10) 6)
         5))
  (is (= (keep-before (interval 5 10) 10)
         (interval 5 9))))

(deftest test-drop-before-1 []
  (is (= (drop-before (interval 5 10) 4)
         (interval 5 10)))
  (is (= (drop-before (interval 1 10) 5)
         (interval 5 10)))
  (is (= (drop-before (interval 5 10) 5)
         (interval 5 10)))
  (is (= (drop-before (interval 5 10) 6)
         (interval 6 10)))
  (is (= (drop-before (interval 5 10) 10)
         10))
  (is (= (drop-before (interval 5 10) 11)
         nil)))

(deftest test-keep-before-2 []
  (is (= (keep-before 1 3)
         1))
  (is (= (keep-before 1 2)
         1))
  (is (= (keep-before 1 1)
         nil)))

(deftest test-drop-before-2 []
  (is (= (drop-before 1 3)
         nil))
  (is (= (drop-before 1 2)
         nil))
  (is (= (drop-before 1 1)
         1))
  (is (= (drop-before 1 0)
         1)))

(deftest test-drop-before-mi-1 []
  (is (= (drop-before (multi-interval 2 4) (lb 3))
         4)))

(deftest test-keep-before-mi-2 []
  (is (= (keep-before (multi-interval 2 4) (lb 3))
         2)))

(deftest test-singleton-interval
  (is (= (interval 1 1) 1)))

(deftest test-interval-<
  (is (interval-< (interval 1 10) (interval 11 20)))
  (is (interval-< 1 (interval 11 20))))

(deftest test-interval->
  (is (interval-> (interval 11 20) (interval 1 10)))
  (is (interval-> (interval 11 20) 1)))

(deftest test-member?-ss-1
  (is (true? (member? 1 1))))

(deftest test-member?-ss-2
  (is (false? (member? 1 2))))

(deftest test-disjoint?-ss-1
  (is (false? (disjoint? 1 1))))

(deftest test-disjoint?-ss-2
  (is (true? (disjoint? 1 2))))

(deftest test-difference-ss-1
  (is (= (difference 1 1)
         nil)))

(deftest test-difference-ss-2
  (is (= (difference 1 2)
         1)))

(deftest test-intersection-ss-1
  (is (= (intersection 1 1)
         1)))

(deftest test-intersection-ss-2
  (is (= (intersection 1 2)
         nil)))

(deftest test-member?-is-1
  (is (true? (member? (interval 1 10) 1))))

(deftest test-member?-si-1
  (is (true? (member? 1 (interval 1 10)))))

(deftest test-disjoint?-is-1
  (is (true? (disjoint? (interval 1 10) 11))))

(deftest test-disjoint?-si-1
  (is (true? (disjoint? 11 (interval 1 10)))))

(deftest test-intersection-is-1
  (is (= (intersection (interval 1 6) 1)
         1)))

(deftest test-intersection-si-1
  (is (= (intersection 1 (interval 1 6))
         1)))

(deftest test-difference-is-1
  (let [mi (difference (interval 1 10) 5)]
    (is (= (first (intervals mi)) (interval 1 4)))
    (is (= (second (intervals mi)) (interval 6 10)))))

(deftest test-difference-si-1
  (let [mi (difference 5 (interval 1 10))]
    (is (= (first (intervals mi)) (interval 1 4)))
    (is (= (second (intervals mi)) (interval 6 10)))))

(deftest test-intersection-ii-1
  (is (= (intersection (interval 1 6) (interval 5 10))
         (interval 5 6))))

(deftest test-intersection-ii-2
  (is (= (intersection (interval 5 10) (interval 1 6))
         (interval 5 6))))

(deftest test-difference-ii-1
  (is (= (difference (interval 1 6) (interval 5 10))
         (interval 1 4))))

(deftest test-difference-ii-2
  (is (= (difference (interval 1 4) (interval 5 10))
         (interval 1 4))))

(deftest test-difference-ii-3
  (is (= (difference (interval 5 10) (interval 1 4))
         (interval 5 10))))

(deftest test-difference-ii-4
  (is (= (difference (interval 1 10) (interval 1 10))
         nil)))

(deftest test-difference-ii-5
  (is (= (difference (interval 2 9) (interval 1 10))
         nil)))

(deftest test-disjoint?-ii-1
  (is (false? (disjoint? (interval 1 6) (interval 5 10))))
  (is (false? (disjoint? (interval 5 10) (interval 1 6))))
  (is (true? (disjoint? (interval 1 6) (interval 10 16))))
  (is (true? (disjoint? (interval 10 16) (interval 1 6)))))

(deftest test-member?-mimi-1
  (is (false? (member? 20 (multi-interval (interval 1 3) 5 (interval 7 10)))))
  (is (false? (member? (multi-interval (interval 1 3) 5 (interval 7 10)) 20))))

(deftest test-disjoint?-mimi-1
  (is (true? (disjoint? 20 (multi-interval (interval 1 3) 5 (interval 7 10)))))
  (is (true? (disjoint? (multi-interval (interval 1 3) 5 (interval 7 10)) 20)))
  (is (true? (disjoint? (interval 20 30) (multi-interval (interval 1 3) 5 (interval 7 10)))))
  (is (true? (disjoint? (multi-interval (interval 1 3) 5 (interval 7 10)) (interval 20 30)))))

(deftest test-equals-mi
  (let [mi0 (multi-interval (interval 1 4) (interval 6 10))
        mi1 (multi-interval (interval 1 4) (interval 6 10))]
    (is (= mi0 mi1))))

;; -----------------------------------------------------------------------------
;; MultiIntervalFD Intersection

(deftest test-intersection-mimi-1
  (let [mi0 (multi-interval (interval 1 4) (interval 6 10))
        mi1 (multi-interval (interval 9 13) (interval 17 20))]
    (is (= (intersection mi0 mi1) (interval 9 10)))
    (is (= (intersection mi1 mi0) (interval 9 10)))))

(deftest test-intersection-mimi-2
  (let [mi0 (multi-interval (interval 1 4) (interval 6 10))]
    (is (= (intersection mi0 7) 7))
    (is (= (intersection 7 mi0) 7))))

;; |-----| 
;;   |-----|
(deftest test-intersection-mimi-3
  (let [mi0 (multi-interval (interval 1 4) (interval 7 10))]
    (is (= (intersection mi0 (interval 3 8))
           (multi-interval (interval 3 4) (interval 7 8))))))

;; |-----|
;;  |---|
(deftest test-intersection-mimi-4
  (let [mi0 (multi-interval (interval 1 4) (interval 7 10))
        mi1 (multi-interval (interval 2 3) (interval 6 9))]
    (is (= (intersection mi0 mi1)
           (multi-interval (interval 2 3) (interval 7 9))))))

;;   |-----|
;; |-----|
(deftest test-intersection-mimi-5
  (let [mi0 (multi-interval (interval 4 8) (interval 12 16))
        mi1 (multi-interval (interval 1 5) (interval 7 15))]
    (is (= (intersection mi0 mi1)
           (multi-interval (interval 4 5) (interval 7 8) (interval 12 15))))))

;;  |---|
;; |-----|
(deftest test-intersection-mimi-6
  (let [mi0 (multi-interval (interval 1 3) (interval 5 6) (interval 8 10))
        mi1 (multi-interval (interval 1 3) (interval 4 7) (interval 8 10))]
    (is (= (intersection mi0 mi1)
           (multi-interval (interval 1 3) (interval 5 6) (interval 8 10))))))

;; |---|  |---|
;; |-------|
(deftest test-intersection-mimi-7
  (let [mi0 (multi-interval (interval 1 4) (interval 7 10))]
    (is (= (intersection mi0 (interval 1 8))
           (multi-interval (interval 1 4) (interval 7 8))))))

;; |--------| |--|
;; |---|  |-------|
(deftest test-intersection-mimi-8
  (let [mi0 (multi-interval (interval 1 7) (interval 9 10))
        mi1 (multi-interval (interval 1 3) (interval 6 11))]
    (is (= (intersection mi0 mi1)
           (multi-interval (interval 1 3) (interval 6 7) (interval 9 10))))))

;; -----------------------------------------------------------------------------
;; MultiIntervalFD Difference

;; |---| |---|
;;         |---| |---|
(deftest test-difference-mimi-1
  (let [mi0 (multi-interval (interval 1 4) (interval 6 10))
        mi1 (multi-interval (interval 9 13) (interval 17 20))]
    (is (= (difference mi0 mi1)
           (multi-interval (interval 1 4) (interval 6 8))))))

;; |---|  |---|
;;         N      
(deftest test-difference-mis-1
  (let [mi0 (multi-interval (interval 1 4) (interval 7 10))]
    (is (= (difference mi0 8)
           (multi-interval (interval 1 4) 7 (interval 9 10))))))

;;       N
;; |---|   |---|
(deftest test-difference-smi-2
  (let [mi0 (multi-interval (interval 1 4) (interval 6 10))]
    (is (= (difference 5 mi0) 5))))

;; |---|   |---|
;;   |-------|
;;
;;   |-------|
;; |---|   |---|
(deftest test-difference-mii-1
  (let [mi0 (multi-interval (interval 1 4) (interval 7 10))]
    (is (= (difference mi0 (interval 3 8))
           (multi-interval (interval 1 2) (interval 9 10))))
    (is (= (difference (interval 3 8) mi0)
           (interval 5 6)))))

;; |---|  |---|
;; |-------| |----|
(deftest test-difference-mimi-2
  (let [mi0 (multi-interval (interval 1 4) (interval 7 10))
        mi1 (multi-interval (interval 1 8) (interval 10 13))]
    (is (= (difference mi0 mi1) 9))))

;;  |----| |-------|
;; |----|    |---|
(deftest test-difference-mimi-3
  (let [mi0 (multi-interval (interval 3 6) (interval 9 15))
        mi1 (multi-interval (interval 1 4) (interval 10 12))]
    (is (= (difference mi0 mi1)
           (multi-interval (interval 5 6) 9 (interval 13 15))))))

;;   |---|     |---|
;; |-----| |-|
(deftest test-difference-mimi-4
  (let [mi0 (multi-interval (interval 3 6) (interval 15 20))
        mi1 (multi-interval (interval 1 6) (interval 10 13))]
    (is (= (difference mi0 mi1)
           (interval 15 20)))))

(deftest test-fd-1
  (let [d (domain 1 2 3)]
    (is (= (lb d) 1))
    (is (= (ub d) 3))))

(deftest test-normalize-intervals-1
  (let [d (domain 1 2 3)]
    (is (= (normalize-intervals (intervals d))
           [(interval 1 3)]))))

(deftest test-normalize-intervals-2
  (let [d (multi-interval (interval 1 4) 5 (interval 6 10))]
    (is (= (normalize-intervals (intervals d))
           [(interval 1 10)]))))

(deftest test-domfd-interval-and-number-1
   (is (= (run* [q]
            (domfd q (interval 1 10))
            (== q 1))
          '(1)))
   (is (= (run* [q]
            (== q 1)
            (domfd q (interval 1 10)))
          '(1))))

(deftest test-domfd-interval-and-number-2
  (is (= (run* [q]
           (domfd q (interval 1 10))
           (== q 11))
         '()))
  (is (= (run* [q]
           (== q 11)
           (domfd q (interval 1 10)))
         '())))

 (deftest test-domfd-many-intervals-1
   (is (= (run* [q]
            (domfd q (interval 1 100))
            (domfd q (interval 30 60))
            (domfd q (interval 50 55))
            (== q 51))
          '(51)))
   (is (= (run* [q]
            (domfd q (interval 1 100))
            (domfd q (interval 30 60))
            (domfd q (interval 50 55))
            (== q 56))
          '())))

(deftest test-process-dom-1
  (let [x (lvar 'x)
        s ((process-dom x 1) empty-s)]
    (is (= (walk s x) 1))))

(deftest test-process-dom-2
  (let [x (lvar 'x)
        s ((process-dom x (interval 1 10)) empty-s)]
    (is (= (get-dom s x) (interval 1 10)))))

(deftest test-domfd-1
  (let [x (lvar 'x)
        s ((domfd x (interval 1 10)) empty-s)]
    (is (= (get-dom s x) (interval 1 10)))))

(deftest test-infd-1
  (let [x (lvar 'x)
        y (lvar 'y)
        f ((infd x y (interval 1 10)) empty-s)
        s (f)]
    (is (= (get-dom s x) (interval 1 10)))
    (is (= (get-dom s y) (interval 1 10)))))

(deftest test-make-fdc-prim-1
  (let [u (lvar 'u)
        w (lvar 'w)
        c (fdc (=fdc u w))]
    (is (= (var-rands c)
           [u w]))
    (is (= (rator c)
           `=fd))
    (is (false? (runnable? c empty-s)))
    (is (true? (relevant? c empty-s)))))

(deftest test-make-fdc-prim-2
  (let [u (lvar 'u)
        v 1
        w (lvar 'w)
        c (+fdc u v w)]
    (is (= (var-rands c)
           [u w]))
    (is (= (rator c)
           `+fd))
    (is (false? (runnable? c empty-s)))
    (is (true? (relevant? c empty-s)))))

(deftest test-make-fdc-1
  (let [u (lvar 'u)
        v 1
        w (lvar 'w)
        c (fdc (+fdc u v w))]
    (is (= (var-rands c)
           [u w]))
    (is (= (rator c)
           `+fd))
    (is (false? (runnable? c empty-s)))
    (is (true? (relevant? c empty-s)))))

(deftest test-addc-1
  (let [u (lvar 'u)
        v 1
        w (lvar 'w)
        c (fdc (+fdc u v w))
        cs (addc (make-cs) c)
        sc (first (constraints-for cs u ::l/fd))]
    (is (= c sc))
    (is (= (id sc) 0))
    (is (= (count (:km cs)) 2))
    (is (= (count (:cm cs)) 1))))

(deftest test-addc-2
  (let [u (lvar 'u)
        v 1
        w (lvar 'w)
        c0 (fdc (+fdc u v w))
        x (lvar 'x)
        c1 (fdc (+fdc w v x))
        cs  (-> (make-cs )
                (addc c0)
                (addc c1))
        sc0 (get (:cm cs) 0)
        sc1 (get (:cm cs) 1)]
    (is (= sc0 c0)) (is (= (id sc0) 0))
    (is (= sc1 c1)) (is (= (id sc1) 1))
    (is (= (id sc0) 0))
    (is (= (count (:km cs)) 3))
    (is (= (count (:cm cs)) 2))))

(deftest test-addcg
  (let [u (lvar 'u)
        v 1
        w (lvar 'w)
        c (fdc (+fdc u v w))
        s ((addcg c) empty-s)]
    (is (= (count (:km (:cs s))) 2))
    (is (= (count (:cm (:cs s))) 1))))

#_(deftest test-purge-c
  (let [u (lvar 'u)
        v 1
        w (lvar 'w)
        c (fdc (+fdc u v w))
        s ((addcg c) empty-s)
        c (first (constraints-for (:cs s) u ::fd))
        s (-> s
            (ext-no-check u 1)
            (ext-no-check w 2))
        s ((checkcg c) s)]
    (is (zero? (count (:km (:cs s)))))
    (is (zero? (count (:cm (:cs s)))))))

(deftest test-=fd-1
  (let [x (lvar 'x)
        y (lvar 'y)
        s ((composeg
            (domfd x (interval 1 6))
            (domfd y (interval 5 10))) empty-s)
        s ((=fd x y) s)
        cs (:cs s)]
    (is (= 2 (count (:km (:cs s))))) ;; works
    (is (= 3 (count (:cm (:cs s)))))
    (is (= (get-dom s x) (interval 5 6)))
    (is (= (get-dom s y) (interval 5 6)))))

(deftest test-multi-interval-1
  (let [mi (multi-interval (interval 1 3) (interval 7 10))]
    (is (= 1 (lb mi)))
    (is (= 10 (ub mi)))))

(deftest test-run-constraints*
  (is (= (run-constraints* [] [] ::subst) s#)))

(deftest test-drop-one-1
  (is (= (:s (drop-one (domain 1 2 3)))
         #{2 3})))

(deftest test-drop-one-2
  (is (= (drop-one (domain 1))
         nil)))

(deftest test-drop-one-3
  (is (= (drop-one 1)
         nil)))

(deftest test-drop-one-4
  (is (= (drop-one (interval 1 10))
         (interval 2 10))))

(deftest test-drop-one-5
  (is (= (drop-one (interval 1 1))
         nil)))

(deftest test-drop-one-6
  (is (= (drop-one (multi-interval (interval 1 10) (interval 15 20)))
         (multi-interval (interval 2 10) (interval 15 20)))))

(deftest test-to-vals-1
  (is (= (to-vals 1) '(1))))

(deftest test-to-vals-2
  (is (= (to-vals (domain 1 2 3)) '(1 2 3))))

(deftest test-to-vals-3
  (is (= (to-vals (interval 1 10))
         '(1 2 3 4 5 6 7 8 9 10))))

(deftest test-to-vals-4
  (is (= (to-vals (multi-interval (interval 1 5) (interval 7 10)))
         '(1 2 3 4 5 7 8 9 10))))

(deftest test-to-vals-5
  (is (= (to-vals (multi-interval (interval 1 5) 7 (interval 9 12)))
         '(1 2 3 4 5 7 9 10 11 12))))

(deftest test-map-sum-1
  (let [x (lvar 'x)
        s ((domfd x (interval 1 10)) empty-s)]
    (is (= (take 10
             (solutions s x
               ((map-sum (fn [v] (updateg x v)))
                (to-vals (interval 1 10)))))
           '(1 2 3 4 5 6 7 8 9 10)))))

(deftest test-force-ans-1
  (let [x (lvar 'x)
        s ((domfd x (interval 1 10)) empty-s)]
    (is (= (take 10
             (solutions s x
               (force-ans x)))
           '(1 2 3 4 5 6 7 8 9 10)))))

(deftest test-force-ans-2
  (let [x (lvar 'x)
        s ((domfd x (interval 1 10)) empty-s)]
    (is (= (take 10
             (solutions s x
               (force-ans [x])))
           '(1 2 3 4 5 6 7 8 9 10)))))

(deftest test-force-ans-3
  (let [x (lvar 'x)
        s ((domfd x (multi-interval (interval 1 4) (interval 6 10)))
            empty-s)]
    (is (= (take 10
             (solutions s x
               (force-ans x)))
           '(1 2 3 4 6 7 8 9 10)))))

(deftest test-verify-all-bound-1
  (let [x (lvar 'x)
        y (lvar 'y)
        s ((composeg
            (domfd x (interval 1 10))
            (domfd y (interval 1 10))) empty-s)]
    (is (nil? (verify-all-bound s [x y])))))

(deftest test-verify-all-bound-2
  (let [x (lvar 'x)
        y (lvar 'y)
        s ((domfd x (interval 1 10)) empty-s)]
    (is (thrown? Exception (verify-all-bound s [x y])))))

(deftest test-enforce-constraints-1
  (let [x (lvar 'x)
        s ((domfd x (interval 1 3)) empty-s)]
    (is (= (solutions s x
             (enforce-constraints x))
           '(1 2 3)))))

(deftest test-reifyg-1
  (let [x (lvar 'x)
        y (lvar 'y)
        s ((composeg
            (domfd x (interval 1 10))
            (domfd y (interval 1 5))) empty-s)
        s ((=fd x y) s)]
    (is (= (take* ((reifyg x) s))
           '(1 2 3 4 5)))))

(deftest test-process-interval-smaller-1
  (let [x (lvar 'x)
        s ((composeg
            (domfd x (interval 1 10))
            (domfd x (interval 2 10))) empty-s)]
    (is (= (get-dom s x)
           (interval 2 10)))))

(deftest test-boundary-interval-1
  (is (difference (interval 1 10) 1)
      (interval 2 10)))

(deftest test-boundary-interval-1
  (is (difference (interval 1 10) 10)
      (interval 1 9)))

(deftest test-process-imi-1
  (let [x (lvar 'x)
        s ((composeg
            (domfd x (interval 2 10))
            (domfd x (multi-interval (interval 1 4) (interval 6 10))))
           empty-s)]
    (is (= (get-dom s x)
           (multi-interval (interval 2 4) (interval 6 10))))))

;; -----------------------------------------------------------------------------
;; cKanren

(deftest test-root-var-1
  (let [x (lvar 'x)
        y (lvar 'y)
        s (-> empty-s
              (ext-no-check x 1)
              (ext-no-check y x))]
    (is (= (root-var s y) x))))

(deftest test-ckanren-1
  (is (= (run* [q]
           (fresh [x]
             (infd x (interval 1 3))
             (== q x)))
         '(1 2 3))))

(deftest test-ckanren-2
  (is (= (run* [q]
           (fresh [x y z]
             (infd x z (interval 1 5))
             (infd y (interval 3 5))
             (+fd x y z)
             (== q [x y z])))
         '([1 3 4] [2 3 5] [1 4 5]))))

(deftest test-ckanren-3
  (is (= (run* [q]
           (fresh [x y]
             (infd x y (interval 1 3))
             (=fd x y)
             (== q [x y])))
         '([1 1] [2 2] [3 3]))))

(deftest test-ckanren-4
  (is (true?
       (every? (fn [[x y]] (not= x y))
         (run* [q]
           (fresh [x y]
             (infd x y (interval 1 10))
             (!=fd x y)
             (== q [x y])))))))

(deftest test-ckanren-5
  (is (= (run* [q]
           (fresh [x y]
             (infd x y (interval 1 3))
             (== x 2)
             (!=fd x y)
             (== q [x y])))
         '([2 1] [2 3]))))

(deftest test-ckanren-6
  (is (= (run* [q]
           (fresh [x]
             (infd x (interval 1 3))
             (+fd x 1 x)
             (== q x)))
         '())))

(deftest test-ckanren-7
  (is (= (run* [q]
           (fresh [x]
             (infd x (interval 1 3))
             (+fd x x x)))
         '())))

(deftest test-ckanren-8
  (is (= (run* [q]
           (fresh [x y]
             (infd x y (interval 1 3))
             (<=fd x y)
             (== q [x y])))
         '([1 1] [1 2] [2 2] [1 3] [3 3] [2 3]))))

(deftest test-ckanren-9
  (is (= (run* [q]
           (fresh [x y]
             (infd x y (interval 1 3))
             (<fd x y)
             (== q [x y])))
         '([1 2] [2 3] [1 3]))))

(defn subgoal [x]
  (fresh [y]
    (== y x)
    (+fd 1 y 3)))

(deftest test-ckanren-10
  (is (= (run* [q]
           (fresh [a]
             (infd a (interval 1 10))
             (subgoal a)
             (== q a)))
         '(2))))

(deftest test-list-sorted
  (is (true? (list-sorted? < [1 2 3])))
  (is (true? (list-sorted? < [1 3 5])))
  (is (false? (list-sorted? < [1 1 3])))
  (is (false? (list-sorted? < [1 5 4 1]))))

(deftest test-with-id
  (let [x (lvar 'x)
        y (lvar 'y)
        n* (sorted-set 1 3 5)
        c (with-id (fdc (-distinctfdc x #{y} (conj n* 7))) 1)]
    (is (= (id c) 1))
    (is (= (id (:proc c)) 1))))

(deftest test-distinctfd
  (is (= (run* [q]
           (fresh [x y z]
             (infd x y z (interval 1 3))
             (distinctfd [x y z])
             (== q [x y z])))
         '([1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1]))))

(deftest test-=fd-1
  (is (= (run* [q]
           (fresh [a b]
             (infd a b (interval 1 3))
             (=fd a b)
             (== q [a b])))
         '([1 1] [2 2] [3 3]))))

(deftest test-!=fd-1
  (is (= (run* [q]
           (fresh [a b]
             (infd a b (interval 1 3))
             (!=fd a b)
             (== q [a b])))
         '([1 2] [1 3] [2 1] [2 3] [3 1] [3 2]))))

(deftest test-<fd-1
  (is (= (run* [q]
           (fresh [a b c]
             (infd a b c (interval 1 3))
             (<fd a b) (<fd b c)
             (== q [a b c])))
         '([1 2 3]))))

(deftest test-<fd-2
  (is (= (run* [q]
           (fresh [x y z]
             (infd x y z (interval 1 10))
             (+fd x y z)
             (<fd x y)
             (== z 10)
             (== q [x y z])))
         '([1 9 10] [2 8 10] [3 7 10] [4 6 10]))))

(deftest test->fd-1
  (is (= (run* [q]
           (fresh [x y z]
             (infd x y z (interval 1 10))
             (+fd x y z)
             (>fd x y)
             (== z 10)
             (== q [x y z])))
         '([6 4 10] [7 3 10] [8 2 10] [9 1 10]))))

(deftest test-<=fd-1
  (is (= (run* [q]
           (fresh [x y]
             (== x 3)
             (infd y (multi-interval 2 4))
             (<=fd x y)
             (== q y)))
         '(4))))

(deftest test->=fd-1
  (is (= (run* [q]
           (fresh [x y]
             (== x 3)
             (infd y (multi-interval 2 4))
             (>=fd x y)
             (== q y)))
         '(2))))

(deftest test-*fd-1
  (is (= (run* [q]
           (fresh [n m]
             (infd n m (interval 1 10))
             (*fd n 2 m)
             (== q [n m])))
         '([1 2] [2 4] [3 6] [4 8] [5 10]))))

(deftest test-*fd-2
  (is (= (run* [q]
           (fresh [n m]
             (infd n m (interval 1 10))
             (*fd n m 10)
             (== q [n m])))
         '([1 10] [2 5] [5 2] [10 1]))))

;; -----------------------------------------------------------------------------
;; CLP(Tree)

(deftest test-recover-vars []
  (let [x (lvar 'x)
        y (lvar 'y)
        s (-> empty-s
              (ext-no-check x 1)
              (ext-no-check y 2))]
    (is (= (recover-vars (:l s))
           #{x y}))))

(deftest test-prefix-s []
  (let [x (lvar 'x)
        y (lvar 'y)
        s empty-s
        sp (-> s
             (ext-no-check x 1)
             (ext-no-check y 2))
        p (prefix-s sp s)]
    (is (= p
           (list (pair y 2) (pair x 1))))
    (is (= (-> p meta :s) sp))))

(deftest test-prefix-subsumes? []
  (let [x (lvar 'x)
        y (lvar 'y)
        z (lvar 'z)
        s empty-s
        sp (-> s
             (ext-no-check x 1)
             (ext-no-check y 2))
        p (prefix-s sp s)]
    (is (true? (prefix-subsumes? p (list (pair x 1)))))
    (is (true? (prefix-subsumes? p (list (pair y 2)))))
    (is (false? (prefix-subsumes? p (list (pair z 3)))))))

(deftest test-remc []
  (let [x (lvar 'x)
        y (lvar 'y)
        z (lvar 'z)
        c (fdc (+fdc x y z))
        cs (addc (make-cs) c)
        cp (get (:cm cs) 0)
        cs (remc cs cp)]
    (is (= (:km cs) {}))
    (is (= (:cm cs) {}))))

(deftest test-treec-id-1 []
  (let [x (lvar 'x)
        y (lvar 'y)
        c (with-id (!=c x y) 0)]
    (is (zero? (id c)))))

(deftest test-tree-constraint? []
  (let [x (lvar 'x)
        y (lvar 'y)
        c (!=c (list (pair x 1) (pair y 2)))
        cs (addc (make-cs) c)]
    (is (tree-constraint? ((:cm cs) 0)))
    (is (= (into #{} (keys (:km cs)))
           #{x y}))))

(deftest test-prefix-protocols []
  (let [x (lvar 'x)
        y (lvar 'y)
        c (!=c (list (pair x 1) (pair y 2)))
        c (with-prefix c (list (pair x 1)))]
    (is (= (prefix c)
           (list (pair x 1))))))

(deftest test-!=-1 []
  (let [x (lvar 'x)
        y (lvar 'y)
        s ((!= x y) empty-s)]
    (is (= (prefix ((:cm (:cs s)) 0)) (list (pair x y))))))

(deftest test-!=-2 []
  (let [x (lvar 'x)
        y (lvar 'y)
        s ((!= x y) empty-s)
        s ((== x y) s)]
    (is (= s nil))))

;; NOTE: we removed -relevant? protocol fn used for purging individual
;; vars from the constraint store. This may return but as a finer grained
;; protocol IRelevantLVar or some such

#_(deftest test-!=-3 []
  (let [x (lvar 'x)
        y (lvar 'y)
        s ((!= x y) empty-s)
        s ((== x 1) s)
        s ((== y 2) s)]
    (is (empty? (:cm (:cs s))))
    (is (empty? (:km (:cs s))))))

(deftest test-!=-4 []
  (let [x (lvar 'x)
        y (lvar 'y)
        s ((== x 1) empty-s)
        s ((== y 2) s)
        s ((!= x y) s)]
    (is (empty? (:cm (:cs s))))
    (is (empty? (:km (:cs s))))))

(deftest test-!=-5 []
  (let [x (lvar 'x)
        y (lvar 'y)
        s ((== x 1) empty-s)
        s ((!= x y) s)
        s ((== y 2) s)]
    (is (empty? (:cm (:cs s))))
    (is (empty? (:km (:cs s))))))

(deftest test-!=-6 []
  (let [x (lvar 'x)
        y (lvar 'y)
        s ((!= x 1) empty-s)]
    (is (= (prefix ((:cm (:cs s)) 0)) (list (pair x 1))))))

#_(deftest test-normalize-store []
  (let [x (lvar 'x)
        y (lvar 'y)
        c (!=c (list (pair x 1)))
        sc (!=c (list (pair x 1) (pair y 2)))
        cs (addc (make-cs) c)]
    ))

(deftest test-multi-constraints-1 []
  (is (= (run* [q]
           (fresh [x y z]
             (infd x y z (interval 1 3))
             (!= z 3)
             (+fd x y z)
             (== q [x y z])))
         '([1 1 2]))))

(deftest test--fd-1 []
  (is (= (run* [q]
           (infd q (interval 1 10))
           (-fd 4 q 1))
         '(3)))
  (is (= (run* [q]
           (infd q (interval 1 10))
           (-fd 4 2 q))
         '(2))))

(deftest test-quotfd-1 []
  (is (= (run* [q]
           (infd q (interval 1 10))
           (quotfd 4 2 q))
         '(2))))

;; =============================================================================
;; eqfd

(deftest test-eqfd-1 []
  (is (= (run* [q]
           (fresh [x y]
             (infd x y (interval 0 9))
             (eqfd
              (= (+ x y) 9)
              (= (+ (* x 2) (* y 4)) 24))
             (== q [x y])))
         '([6 3]))))

;; FIXME

(deftest test-eqfd-2 []
  (is (= (run* [q]
           (fresh [s e n d m o r y]
             (== q [s e n d m o r y])
             (infd s e n d m o r y (interval 0 9))
             (distinctfd [s e n d m o r y])
             (!=fd m 0) (!=fd s 0)
             (eqfd
              (= (+ (* 1000 s) (* 100 e) (* 10 n) d
                    (* 1000 m) (* 100 o) (* 10 r) e)
                 (+ (* 10000 m) (* 1000 o) (* 100 n) (* 10 e) y)))))
         '([9 5 6 7 1 0 8 2]))))

(deftest test-eqfd-3 []
  (is (= (run* [q]
           (fresh [x y]
             (infd x y (interval 1 20))
             (eqfd
              (= (+ x y) 11)
              (= (- (* 3 x) y) 5))
             (== q [x y])))
         '([4 7]))))

(deftest test-distinctfd-1 []
  (is (= (run 1 [q]
           (fresh [x y]
             (distinctfd q)
             (== q [x y])
             (== x 1)
             (== y 1)))
         ())))

(deftest test-logic-62-fd []
  (is (= (run 1 [q]
           (fresh [x y a b]
             (distinctfd [x y])
             (== [x y] [a b])
             (== q [a b])
             (== a 1)
             (== b 1)))
         ()))
  (is (= (run 1 [q]
           (fresh [x y a b]
             (== [x y] [a b])
             (== q [a b])
             (== a 1)
             (== b 1)
             (distinctfd [x y])))
         ())))

(deftest test-distincto-1 []
  (is (= (run 1 [q]
           (fresh [x y a b]
             (distincto q)
             (== [x y] [a b])
             (== q [a b])
             (== x 1)
             (== y 2)))
         '([1 2]))))

(deftest test-eq-vars-1 []
  (let [x0 (lvar 'x)
        x1 (with-meta x0 {:foo 'bar})
        s  (unify empty-s x0 x1)]
    (is (= s empty-s))))

;; =============================================================================
;; predc

(deftest test-predc-1 []
  (is (= (run* [q]
           (predc q number? `number?))
         '((_.0 :- clojure.core/number?))))
  (is (= (run* [q]
           (predc q number? `number?)
           (== q 1))
         '(1)))
  (is (= (run* [q]
           (== q 1)
           (predc q number? `number?))
         '(1)))
  (is (= (run* [q]
           (predc q number? `number?)
           (== q "foo"))
         ()))
  (is (= (run* [q]
           (== q "foo")
           (predc q number? `number?))
         ())))

;; =============================================================================
;; extensible unifier

(deftest test-extensible-unifier-1
  (is (= (unifier '(^{::l/ann ::l/numeric} ?x) '(1))
         '(1)))
  (is (= (unifier '(^{::l/ann ::l/numeric} ?x) '("foo"))
         nil)))

;; =============================================================================
;; Implementation Specific Tests - Subject To Change

(deftest test-attrs-1 []
  (let [x (lvar 'x)
        s (add-attr empty-s x :foo 'bar)]
    (is (= (get-attr s x :foo) 'bar))))

(deftest test-attrs-2 []
  (let [x (lvar 'x)
        s (ext-no-check empty-s x 1)
        s (add-attr s x :foo 'bar)
        s (add-attr s x :baz 'woz)]
    (is (= (get-attr s x :foo) 'bar))
    (is (= (get-attr s x :baz) 'woz))))

(deftest test-attrs-2 []
  (let [x (lvar 'x)
        s (ext-no-check empty-s x 1)
        s (add-attr s x :foo 'bar)
        s (add-attr s x :baz 'woz)
        s (rem-attr s x :foo)]
    (is (= (get-attr s x :foo) nil))))

(deftest test-root-1 []
  (let [x (lvar 'x)
        s (ext-no-check empty-s x 1)]
    (= (root-var s x) x)
    (= (root-val s x) 1)))

(deftest test-root-2 []
  (let [x (lvar 'x)
        s (add-attr empty-s x :foo 'bar)]
    (is (subst-val? (root-val s x)))))

(deftest test-root-3 []
  (let [x (lvar 'x)
        y (lvar 'y)
        s (-> empty-s
           (ext-no-check x 1)
           (ext-no-check y x))]
    (is (= (root-var s y) x))))

(deftest test-update-1 []
  (let [x (lvar 'x)
        s (ext-no-check empty-s x (subst-val ::l/unbound))
        s (add-attr s x ::fd (domain 1 2 3))
        s (update s x 1)]
    (is (= (:v (root-val s x)) 1))
    (is (= (get-attr s x ::fd) (domain 1 2 3)))
    (is (= (walk s x) 1))))
