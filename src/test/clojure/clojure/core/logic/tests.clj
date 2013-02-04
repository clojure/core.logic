(ns clojure.core.logic.tests
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        clojure.test)
  (:require [clojure.core.logic.fd :as fd])
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
         '_2)))

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
          '[[_0 _1]])))

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
  (is (=  (into #{}
            (run* [x]
              (conde
                [(== x 'olive) succeed]
                [succeed succeed]
                [(== x 'oil) succeed])))
          (into #{}
            '[olive _0 oil]))))

(deftest test-basic-conde-2
  (is (= (into #{}
           (run* [r]
             (fresh [x y]
               (conde
                 [(== 'split x) (== 'pea y)]
                 [(== 'navy x) (== 'bean y)])
               (== (cons x (cons y ())) r))))
         (into #{}
           '[(split pea) (navy bean)]))))

(defn teacupo [x]
  (conde
    [(== 'tea x) s#]
    [(== 'cup x) s#]))

(deftest test-basic-conde-e-3
  (is (= (into #{}
           (run* [r]
             (fresh [x y]
               (conde
                 [(teacupo x) (== true y) s#]
                 [(== false x) (== true y)])
               (== (cons x (cons y ())) r))))
         (into #{} '((false true) (tea true) (cup true))))))

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
         '[(_0 1 2)])))

(deftest test-resto-2
  (is (= (run* [q]
           (resto q [1 2]))
         '[(_0 1 2)])))

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
  (is (= (into #{}
           (run* [x]
             (flatteno '[[a b] c] x)))
         (into #{}
           '(([[a b] c]) ([a b] (c)) ([a b] c) ([a b] c ())
             (a (b) (c)) (a (b) c) (a (b) c ()) (a b (c))
             (a b () (c)) (a b c) (a b c ()) (a b () c)
             (a b () c ()))))))

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
  (is (= (into #{}
           (run* [q]
             (all
              (== q [(lvar) (lvar)])
              (membero ['foo (lvar)] q)
              (membero [(lvar) 'bar] q))))
         (into #{}
           '([[foo bar] _0] [[foo _0] [_1 bar]]
               [[_0 bar] [foo _1]] [_0 [foo bar]])))))

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
  (is (= (into #{}
           (run* [q]
             (fresh [x y]
               (digit-4 x)
               (digit-4 y)
               (== q [x y]))))
         (into #{}
           '([0 0] [0 1] [0 2] [1 0] [0 3] [1 1] [1 2] [2 0]
               [1 3] [2 1] [3 0] [2 2] [3 1] [2 3] [3 2] [3 3])))))

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
         '(_0))))

(deftest test-divergence-2
  (is (= (run 1 [q]
           (conde
             [f1 (== false false)]
             [(== false false)]))
         '(_0))))

(def f2
  (fresh []
    (conde
      [f2 (conde
            [f2] 
            [(== false false)])]
      [(== false false)])))

(deftest test-divergence-3
  (is (= (run 5 [q] f2)
         '(_0 _0 _0 _0 _0))))

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
  (is (= (into #{}
           (run* [r]
             (conde
               [(teacupo r) s#]
               [(== false r) s#])))
         (into #{}
           '(false tea cup)))))

(deftest test-condu-3
  (is (= (into #{}
           (run* [r]
             (conda
               [(teacupo r) s#]
               [(== false r) s#])))
         (into #{} '(tea cup)))))

;; -----------------------------------------------------------------------------
;; disequality

(deftest test-disequality-1
  (is (= (run* [q]
           (fresh [x]
             (!= x 1)
             (== q x)))
         '((_0 :- (!= (_0 1)))))))

(deftest test-disequality-2
  (is (= (run* [q]
           (fresh [x]
             (== q x)
             (!= x 1)))
         '((_0 :- (!= (_0 1)))))))

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
         '(([_0 _1] :- (!= (_1 2)) (!= (_0 1)))))))

(deftest test-disequality-16
  (is (= (run* [q]
           (fresh [x y z]
             (== y [z])
             (!= [z] x)
             (== z 'foo)
             (== x ['foo])))
         '())))

(deftest test-disequality-17
  (is (= (run* [q]
           (fresh [x y]
             (!= [1 x] [y 2])
             (== q [x y])))
         '(([_0 _1] :- (!= (_0 2) (_1 1))))))
  (is (= (run* [q]
           (fresh [x y]
             (!= [x 1] [2 y])))
         '((_0 :- (!= (_1 1) (_2 2)))))))

(deftest test-logic-95-disequality-1
  (is (= (run* [q]
           (fresh [x y w z]
             (!= x y)
             (!= w z)
             (== z y)
             (== x 'foo)
             (== y 'foo)))
        ())))

(deftest test-logic-95-disequality-2
  (is (= (run* [q]
           (fresh [x y w z]
             (!= x [y])
             (== x ['foo])
             (== y 'foo)))
        ())))

(deftest test-logic-96-disequality-1
  (is (= (run* [q]
           (fresh [x y z]
             (!= x [y])
             (== x [z])
             (== y 'foo)
             (== z 'bar)))
        '(_0))))

(deftest test-logic-100-disequality-1
  (is (= (run* [q]
           (fresh [a b]
             (== q [a b])
             (!= a q)
             (== a 1)))
        '([1 _0]))))

(deftest test-logic-100-disequality-2
  (is (= (run* [q]
           (fresh [a b]
             (!= a q)
             (== q [a b])))
        '([_0 _1])))
  (is (= (run* [q]
           (fresh [a b]
             (== q [a b])
             (!= a q)))
        '([_0 _1]))))

(deftest test-logic-100-disequality-3
  (is (= (run* [q]
           (fresh [x y w z]
             (== x [1 w])
             (== y [2 z])
             (!= x y)))
        '(_0)))
  (is (= (run* [q]
           (fresh [x y w z]
             (!= x y)
             (== x [1 w])
             (== y [2 z])))
        '(_0))))
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
  (is (= (into #{} (run* [q] (patho :a q)))
         (into #{} '(:b :a :d)))))

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
  (is (= (into #{}
           (run* [q]
             (fresh [x y]
               (likes x y)
               (== q [x y]))))
         (into #{} '([John Martha] [Ricky Lucy])))))

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
         (lcons 1 '?z))))

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

;; -----------------------------------------------------------------------------
;; Unifier with constraints

(defnc evenc [x]
  (even? x))

(deftest test-unifier-constraints-1 ;;One var
  (is (= (unifier '{:a ?a} {:a 2} :when {'?a evenc})
         {:a 2}))
  (is (= (unifier '{:a ?a} {:a 1} :when {'?a evenc})
         nil)))

(deftest test-unifier-constraints-2 ;;Two vars
  (is (= (unifier '{:a ?a :b ?b} {:a 2 :b 2} :when {'?a evenc '?b evenc})
         {:a 2 :b 2}))
  (is (= (unifier '{:a ?a :b ?b} {:a 1 :b 2} :when {'?a evenc '?b evenc})
         nil)))

;;Anonymous constraints
(deftest test-unifier-constraints-3 ;;One var
  (is (= (unifier '{:a ?a} {:a 2} :when {'?a (fnc [x] (even? x))})
         {:a 2}))
  (is (= (unifier '{:a ?a} {:a 1} :when {'?a (fnc [x] (even? x))})
         nil)))


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
         '{?x 1, ?y ?z})))

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
;; Pattern Matching

(defne pm1 [x y]
  ([:foo :bar]))

(defne pm2 [x y]
  ([_ x]))

(defne pm3 [x y]
  ([_ 'x]))

(defne pm4 [x y]
  ([[h . t] t]))

(deftest test-pm []
  (is (= (run* [q] (fresh [x y] (== q [x y]) (pm1 x y))) '([:foo :bar])))
  (is (= (run* [q] (fresh [x y] (pm2 x y) (== x y))) '(_0)))
  (is (= (run* [q] (pm4 '(1 2) q)) '((2)))))

(defne form->ast1 [form ast]
  (['(fn ~args . ~body) {:op :fn :args args :body body}]))

(defne form->ast2 [form ast]
  (['(fn [~f . ~rest] . ~body) {:op :fn :f f :rest rest :body body}]))

(deftest test-code-match-1
  (is (= (run* [q]
           (form->ast1 '(fn [x y] (+ x y)) q))
         '({:op :fn :args [x y] :body ((+ x y))})))
  (is (= (run* [q]
           (form->ast2 '(fn [x y] (+ x y)) q))
         '({:op :fn :f x :rest (y) :body ((+ x y))}))))

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

;; REMOVED - fn application no longer supported in patterns, list syntax is
;; much more useful when used for matching Clojure source - David

;; (defn s [n] (llist n []))

;; (def zero 0)
;; (def one (s zero))
;; (def two (s one))
;; (def three (s two))
;; (def four (s three))
;; (def five (s four))
;; (def six  (s five))

;; (defn natural-number [x]
;;   (matche [x]
;;     ([zero])
;;     ([(s y)] (natural-number y))))

;; (deftest test-matche-with-expr
;;   (is (= (run* [q] (natural-number one))
;;          '(_0 _0))))

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
  (is (= (unifier '{:a ?x} '{:a ?y} '{:a 5})
         {:a 5}))
  (is (= (unifier '{:a ?x} '{:a 5} '{:a ?y})
         {:a 5})))

(deftest test-34-unify-with-metadata
  (is (run* [q]
            (== q (quote ^:haz-meta-daytuhs (form form form))))
      '((^:haz-meta-daytuhs (form form form)))))

(deftest test-42-multiple-run-parameters
  (is (= '[[3 _0 [3 _0]]]
         (run* [x y z]
           (== z [x y])
           (== [x] [3])))))

(deftest test-49-partial-map-unification
  (is (= [1]
         (run* [q]
           (fresh [x]
             (== {:a 1 :b 2} (partial-map {:a x}))
             (== q x)))))
  (is (= [1]
         (run* [q]
           (fresh [x]
             (== (partial-map {:a x}) {:a 1 :b 2})
             (== q x))))))

(deftest test-73-partial-map-unification
  (is (= (run* [q]
           (fresh [a]
             (== {:x 1} (partial-map {:a a}))))
         '()))
  (is (= (run* [q]
           (== {:a 1} (partial-map {:a q})))
         '(1))))

(deftest test-75-map-sum-maps-lcons
  (is (= (into #{}
           (run* [q]
             (fresh [x]
               (fd/in x (fd/interval 1 3))
               (== q {:foo x}))))
         (into #{} '({:foo 1} {:foo 2} {:foo 3}))))
  (is (= (into #{}
           (run* [q]
             (fresh [x y]
               (fd/in x (fd/interval 1 3))
               (== q (lcons x y)))))
         (into #{} [(lcons 1 '_0) (lcons 2 '_0) (lcons 3 '_0)]))))

(deftest test-82-nil-lcons-tail
  (is (= (run 1 [q]
           (fresh [a b c]
             (conso a b c)
             (== b nil)
             (== `(~a) c)))
         '(_0))))

(deftest test-85-alias
  (is (= (run* [q]
           (fresh [x y]
             (predc y even? `even?)
             (predc x odd? `odd)
             (== x y)
             (== x 1)
             (== q [x y])))
         ())))

(deftest test-77-alias
  (is (= (run 1 [r a b x]
           (== r [a b])
           (fd/in a b x (fd/domain 1 2))
           (fd/< a b)
           (firsto r x))
         '([[1 2] 1 2 1]))))

(deftest test-90-<-diverge
  (is (= (run 1 [a b c d]
           (fd/in a b c d (fd/interval 0 4))
           (fd/< a b)
           (fd/< c d)
           (fd/< d a))
         '([2 3 0 1]))))

(deftest test-103-<=-diverge
  (is (= (into #{}
           (run 2 [a b s p]
             (fd/in a b (fd/interval 2 99))
             (fd/>= a b)
             (fd/+ a b s)
             (fd/* a b p)))
         (into #{} '([2 2 4 4] [3 2 5 6])))))

(defrecord RecordTest [a b]
  IUninitialized
  (-uninitialized [_]
    (RecordTest. nil nil)))

(deftest test-53-lossy-records
  (is (= (run* [q]
           (== q (RecordTest. 1 2)))
         (list #clojure.core.logic.tests.RecordTest{:a 1, :b 2}))))

;; =============================================================================
;; cKanren

(deftest test-pair []
  (is (= (pair 1 2)
         (pair 1 2))))

(deftest test-dom-1 []
  (let [x (lvar 'x)
        s ((fd/dom x 1) empty-s)]
    (is (= (:s s) {x 1}))))

#_(deftest test-dom-2 []
  (let [x (lvar 'x)
        s ((fd/dom x (fd/interval 1 10)) empty-s)]
    (is (= (:ws s) {x (fd/interval 1 10)}))))

#_(deftest test-dom-3 []
  (let [x (lvar 'x)
        s ((composeg
            (fd/dom x (fd/interval 1 10))
            (fd/dom x (fd/interval 3 6))) empty-s)]
    (is (= (:ws s) {x (fd/interval 3 6)}))))

#_(deftest test-dom-4 []
  (let [x (lvar 'x)
        s ((composeg
            (fd/dom x (fd/interval 1 5))
            (fd/dom x (fd/interval 5 9))) empty-s)]
    (is (= (:s s) {x 5}))
    (is (= (:ws s) {}))))

(deftest test-keep-before-1 []
  (is (= (fd/keep-before (fd/interval 1 10) 5)
         (fd/interval 1 4)))
  (is (= (fd/keep-before (fd/interval 5 10) 5)
         nil))
  (is (= (fd/keep-before (fd/interval 5 10) 6)
         5))
  (is (= (fd/keep-before (fd/interval 5 10) 10)
         (fd/interval 5 9))))

(deftest test-drop-before-1 []
  (is (= (fd/drop-before (fd/interval 5 10) 4)
         (fd/interval 5 10)))
  (is (= (fd/drop-before (fd/interval 1 10) 5)
         (fd/interval 5 10)))
  (is (= (fd/drop-before (fd/interval 5 10) 5)
         (fd/interval 5 10)))
  (is (= (fd/drop-before (fd/interval 5 10) 6)
         (fd/interval 6 10)))
  (is (= (fd/drop-before (fd/interval 5 10) 10)
         10))
  (is (= (fd/drop-before (fd/interval 5 10) 11)
         nil)))

(deftest test-keep-before-2 []
  (is (= (fd/keep-before 1 3)
         1))
  (is (= (fd/keep-before 1 2)
         1))
  (is (= (fd/keep-before 1 1)
         nil)))

(deftest test-drop-before-2 []
  (is (= (fd/drop-before 1 3)
         nil))
  (is (= (fd/drop-before 1 2)
         nil))
  (is (= (fd/drop-before 1 1)
         1))
  (is (= (fd/drop-before 1 0)
         1)))

(deftest test-drop-before-mi-1 []
  (is (= (fd/drop-before (fd/multi-interval 2 4) (fd/lb 3))
         4)))

(deftest test-keep-before-mi-2 []
  (is (= (fd/keep-before (fd/multi-interval 2 4) (fd/lb 3))
         2)))

(deftest test-singleton-interval
  (is (= (fd/interval 1 1) 1)))

(deftest test-interval-<
  (is (fd/interval-< (fd/interval 1 10) (fd/interval 11 20)))
  (is (fd/interval-< 1 (fd/interval 11 20))))

(deftest test-interval->
  (is (fd/interval-> (fd/interval 11 20) (fd/interval 1 10)))
  (is (fd/interval-> (fd/interval 11 20) 1)))

(deftest test-member?-ss-1
  (is (true? (fd/member? 1 1))))

(deftest test-member?-ss-2
  (is (false? (fd/member? 1 2))))

(deftest test-disjoint?-ss-1
  (is (false? (fd/disjoint? 1 1))))

(deftest test-disjoint?-ss-2
  (is (true? (fd/disjoint? 1 2))))

(deftest test-difference-ss-1
  (is (= (fd/difference 1 1)
         nil)))

(deftest test-difference-ss-2
  (is (= (fd/difference 1 2)
         1)))

(deftest test-intersection-ss-1
  (is (= (fd/intersection 1 1)
         1)))

(deftest test-intersection-ss-2
  (is (= (fd/intersection 1 2)
         nil)))

(deftest test-member?-is-1
  (is (true? (fd/member? (fd/interval 1 10) 1))))

(deftest test-member?-si-1
  (is (true? (fd/member? 1 (fd/interval 1 10)))))

(deftest test-disjoint?-is-1
  (is (true? (fd/disjoint? (fd/interval 1 10) 11))))

(deftest test-disjoint?-si-1
  (is (true? (fd/disjoint? 11 (fd/interval 1 10)))))

(deftest test-intersection-is-1
  (is (= (fd/intersection (fd/interval 1 6) 1)
         1)))

(deftest test-intersection-si-1
  (is (= (fd/intersection 1 (fd/interval 1 6))
         1)))

(deftest test-difference-is-1
  (let [mi (fd/difference (fd/interval 1 10) 5)]
    (is (= (first (fd/intervals mi)) (fd/interval 1 4)))
    (is (= (second (fd/intervals mi)) (fd/interval 6 10)))))

(deftest test-difference-si-1
  (let [mi (fd/difference 5 (fd/interval 1 10))]
    (is (= (first (fd/intervals mi)) (fd/interval 1 4)))
    (is (= (second (fd/intervals mi)) (fd/interval 6 10)))))

(deftest test-intersection-ii-1
  (is (= (fd/intersection (fd/interval 1 6) (fd/interval 5 10))
         (fd/interval 5 6))))

(deftest test-intersection-ii-2
  (is (= (fd/intersection (fd/interval 5 10) (fd/interval 1 6))
         (fd/interval 5 6))))

(deftest test-difference-ii-1
  (is (= (fd/difference (fd/interval 1 6) (fd/interval 5 10))
         (fd/interval 1 4))))

(deftest test-difference-ii-2
  (is (= (fd/difference (fd/interval 1 4) (fd/interval 5 10))
         (fd/interval 1 4))))

(deftest test-difference-ii-3
  (is (= (fd/difference (fd/interval 5 10) (fd/interval 1 4))
         (fd/interval 5 10))))

(deftest test-difference-ii-4
  (is (= (fd/difference (fd/interval 1 10) (fd/interval 1 10))
         nil)))

(deftest test-difference-ii-5
  (is (= (fd/difference (fd/interval 2 9) (fd/interval 1 10))
         nil)))

(deftest test-disjoint?-ii-1
  (is (false? (fd/disjoint? (fd/interval 1 6) (fd/interval 5 10))))
  (is (false? (fd/disjoint? (fd/interval 5 10) (fd/interval 1 6))))
  (is (true? (fd/disjoint? (fd/interval 1 6) (fd/interval 10 16))))
  (is (true? (fd/disjoint? (fd/interval 10 16) (fd/interval 1 6)))))

(deftest test-member?-mimi-1
  (is (false? (fd/member? 20 (fd/multi-interval (fd/interval 1 3) 5 (fd/interval 7 10)))))
  (is (false? (fd/member? (fd/multi-interval (fd/interval 1 3) 5 (fd/interval 7 10)) 20))))

(deftest test-disjoint?-mimi-1
  (is (true? (fd/disjoint? 20 (fd/multi-interval (fd/interval 1 3) 5 (fd/interval 7 10)))))
  (is (true? (fd/disjoint? (fd/multi-interval (fd/interval 1 3) 5 (fd/interval 7 10)) 20)))
  (is (true? (fd/disjoint? (fd/interval 20 30) (fd/multi-interval (fd/interval 1 3) 5 (fd/interval 7 10)))))
  (is (true? (fd/disjoint? (fd/multi-interval (fd/interval 1 3) 5 (fd/interval 7 10)) (fd/interval 20 30)))))

(deftest test-equals-mi
  (let [mi0 (fd/multi-interval (fd/interval 1 4) (fd/interval 6 10))
        mi1 (fd/multi-interval (fd/interval 1 4) (fd/interval 6 10))]
    (is (= mi0 mi1))))

;; -----------------------------------------------------------------------------
;; MultiFd/IntervalFD Intersection

(deftest test-intersection-mimi-1
  (let [mi0 (fd/multi-interval (fd/interval 1 4) (fd/interval 6 10))
        mi1 (fd/multi-interval (fd/interval 9 13) (fd/interval 17 20))]
    (is (= (fd/intersection mi0 mi1) (fd/interval 9 10)))
    (is (= (fd/intersection mi1 mi0) (fd/interval 9 10)))))

(deftest test-intersection-mimi-2
  (let [mi0 (fd/multi-interval (fd/interval 1 4) (fd/interval 6 10))]
    (is (= (fd/intersection mi0 7) 7))
    (is (= (fd/intersection 7 mi0) 7))))

;; |-----| 
;;   |-----|
(deftest test-intersection-mimi-3
  (let [mi0 (fd/multi-interval (fd/interval 1 4) (fd/interval 7 10))]
    (is (= (fd/intersection mi0 (fd/interval 3 8))
           (fd/multi-interval (fd/interval 3 4) (fd/interval 7 8))))))

;; |-----|
;;  |---|
(deftest test-intersection-mimi-4
  (let [mi0 (fd/multi-interval (fd/interval 1 4) (fd/interval 7 10))
        mi1 (fd/multi-interval (fd/interval 2 3) (fd/interval 6 9))]
    (is (= (fd/intersection mi0 mi1)
           (fd/multi-interval (fd/interval 2 3) (fd/interval 7 9))))))

;;   |-----|
;; |-----|
(deftest test-intersection-mimi-5
  (let [mi0 (fd/multi-interval (fd/interval 4 8) (fd/interval 12 16))
        mi1 (fd/multi-interval (fd/interval 1 5) (fd/interval 7 15))]
    (is (= (fd/intersection mi0 mi1)
           (fd/multi-interval (fd/interval 4 5) (fd/interval 7 8) (fd/interval 12 15))))))

;;  |---|
;; |-----|
(deftest test-intersection-mimi-6
  (let [mi0 (fd/multi-interval (fd/interval 1 3) (fd/interval 5 6) (fd/interval 8 10))
        mi1 (fd/multi-interval (fd/interval 1 3) (fd/interval 4 7) (fd/interval 8 10))]
    (is (= (fd/intersection mi0 mi1)
           (fd/multi-interval (fd/interval 1 3) (fd/interval 5 6) (fd/interval 8 10))))))

;; |---|  |---|
;; |-------|
(deftest test-intersection-mimi-7
  (let [mi0 (fd/multi-interval (fd/interval 1 4) (fd/interval 7 10))]
    (is (= (fd/intersection mi0 (fd/interval 1 8))
           (fd/multi-interval (fd/interval 1 4) (fd/interval 7 8))))))

;; |--------| |--|
;; |---|  |-------|
(deftest test-intersection-mimi-8
  (let [mi0 (fd/multi-interval (fd/interval 1 7) (fd/interval 9 10))
        mi1 (fd/multi-interval (fd/interval 1 3) (fd/interval 6 11))]
    (is (= (fd/intersection mi0 mi1)
           (fd/multi-interval (fd/interval 1 3) (fd/interval 6 7) (fd/interval 9 10))))))

;; -----------------------------------------------------------------------------
;; MultiFd/IntervalFD Fd/Difference

;; |---| |---|
;;         |---| |---|
(deftest test-difference-mimi-1
  (let [mi0 (fd/multi-interval (fd/interval 1 4) (fd/interval 6 10))
        mi1 (fd/multi-interval (fd/interval 9 13) (fd/interval 17 20))]
    (is (= (fd/difference mi0 mi1)
           (fd/multi-interval (fd/interval 1 4) (fd/interval 6 8))))))

;; |---|  |---|
;;         N      
(deftest test-difference-mis-1
  (let [mi0 (fd/multi-interval (fd/interval 1 4) (fd/interval 7 10))]
    (is (= (fd/difference mi0 8)
           (fd/multi-interval (fd/interval 1 4) 7 (fd/interval 9 10))))))

;;       N
;; |---|   |---|
(deftest test-difference-smi-2
  (let [mi0 (fd/multi-interval (fd/interval 1 4) (fd/interval 6 10))]
    (is (= (fd/difference 5 mi0) 5))))

;; |---|   |---|
;;   |-------|
;;
;;   |-------|
;; |---|   |---|
(deftest test-difference-mii-1
  (let [mi0 (fd/multi-interval (fd/interval 1 4) (fd/interval 7 10))]
    (is (= (fd/difference mi0 (fd/interval 3 8))
           (fd/multi-interval (fd/interval 1 2) (fd/interval 9 10))))
    (is (= (fd/difference (fd/interval 3 8) mi0)
           (fd/interval 5 6)))))

;; |---|  |---|
;; |-------| |----|
(deftest test-difference-mimi-2
  (let [mi0 (fd/multi-interval (fd/interval 1 4) (fd/interval 7 10))
        mi1 (fd/multi-interval (fd/interval 1 8) (fd/interval 10 13))]
    (is (= (fd/difference mi0 mi1) 9))))

;;  |----| |-------|
;; |----|    |---|
(deftest test-difference-mimi-3
  (let [mi0 (fd/multi-interval (fd/interval 3 6) (fd/interval 9 15))
        mi1 (fd/multi-interval (fd/interval 1 4) (fd/interval 10 12))]
    (is (= (fd/difference mi0 mi1)
           (fd/multi-interval (fd/interval 5 6) 9 (fd/interval 13 15))))))

;;   |---|     |---|
;; |-----| |-|
(deftest test-difference-mimi-4
  (let [mi0 (fd/multi-interval (fd/interval 3 6) (fd/interval 15 20))
        mi1 (fd/multi-interval (fd/interval 1 6) (fd/interval 10 13))]
    (is (= (fd/difference mi0 mi1)
           (fd/interval 15 20)))))

(deftest test-fd-1
  (let [d (fd/domain 1 2 3)]
    (is (= (fd/lb d) 1))
    (is (= (fd/ub d) 3))))

(deftest test-normalize-intervals-1
  (let [d (fd/domain 1 2 3)]
    (is (= (fd/normalize-intervals (fd/intervals d))
           [(fd/interval 1 3)]))))

(deftest test-normalize-intervals-2
  (let [d (fd/multi-interval (fd/interval 1 4) 5 (fd/interval 6 10))]
    (is (= (fd/normalize-intervals (fd/intervals d))
           [(fd/interval 1 10)]))))

(deftest test-dom-interval-and-number-1
   (is (= (run* [q]
            (fd/dom q (fd/interval 1 10))
            (== q 1))
          '(1)))
   (is (= (run* [q]
            (== q 1)
            (fd/dom q (fd/interval 1 10)))
          '(1))))

(deftest test-dom-interval-and-number-2
  (is (= (run* [q]
           (fd/dom q (fd/interval 1 10))
           (== q 11))
         '()))
  (is (= (run* [q]
           (== q 11)
           (fd/dom q (fd/interval 1 10)))
         '())))

 (deftest test-dom-many-intervals-1
   (is (= (run* [q]
            (fd/dom q (fd/interval 1 100))
            (fd/dom q (fd/interval 30 60))
            (fd/dom q (fd/interval 50 55))
            (== q 51))
          '(51)))
   (is (= (run* [q]
            (fd/dom q (fd/interval 1 100))
            (fd/dom q (fd/interval 30 60))
            (fd/dom q (fd/interval 50 55))
            (== q 56))
          '())))

(deftest test-process-dom-1
  (let [x (lvar 'x)
        s ((fd/process-dom x 1) empty-s)]
    (is (= (walk s x) 1))))

(deftest test-process-dom-2
  (let [x (lvar 'x)
        s ((fd/process-dom x (fd/interval 1 10)) empty-s)]
    (is (= (fd/get-dom s x) (fd/interval 1 10)))))

(deftest test-dom-1
  (let [x (lvar 'x)
        s ((fd/dom x (fd/interval 1 10)) empty-s)]
    (is (= (fd/get-dom s x) (fd/interval 1 10)))))

(deftest test-in-1
  (let [x (lvar 'x)
        y (lvar 'y)
        f ((fd/in x y (fd/interval 1 10)) empty-s)
        s (f)]
    (is (= (fd/get-dom s x) (fd/interval 1 10)))
    (is (= (fd/get-dom s y) (fd/interval 1 10)))))

(deftest test-make-fdc-prim-1
  (let [u (lvar 'u)
        w (lvar 'w)
        c (fd/==c u w)]
    (is (= (var-rands empty-s c)
           [u w]))
    (is (= (rator c)
           'clojure.core.logic.fd/==))
    (is (false? (runnable? c empty-s)))
    (is (true? (relevant? c empty-s)))))

(deftest test-make-fdc-prim-2
  (let [u (lvar 'u)
        v 1
        w (lvar 'w)
        c (fd/+c u v w)]
    (is (= (var-rands empty-s c)
           [u w]))
    (is (= (rator c)
           'clojure.core.logic.fd/+))
    (is (false? (runnable? c empty-s)))
    (is (true? (relevant? c empty-s)))))

(deftest test-make-fdc-1
  (let [u (lvar 'u)
        v 1
        w (lvar 'w)
        c (fd/+c u v w)]
    (is (= (var-rands empty-s c)
           [u w]))
    (is (= (rator c)
           `fd/+))
    (is (false? (runnable? c empty-s)))
    (is (true? (relevant? c empty-s)))))

(deftest test-addc-1
  (let [u (lvar 'u)
        v 1
        w (lvar 'w)
        c (fd/+c u v w)
        cs (addc (make-cs) empty-s c)
        sc (first (constraints-for cs empty-s u ::l/fd))]
    (is (= (id sc) 0))
    (is (= (count (:km cs)) 2))
    (is (= (count (:cm cs)) 1))))

(deftest test-addc-2
  (let [u (lvar 'u)
        v 1
        w (lvar 'w)
        c0 (fd/+c u v w)
        x (lvar 'x)
        c1 (fd/+c w v x)
        cs  (-> (make-cs)
                (addc empty-s c0)
                (addc empty-s c1))
        sc0 (get (:cm cs) 0)
        sc1 (get (:cm cs) 1)]
    (is (= (id sc0) 0))
    (is (= (id sc1) 1))
    (is (= (count (:km cs)) 3))
    (is (= (count (:cm cs)) 2))))

(deftest test-addcg
  (let [u (lvar 'u)
        v 1
        w (lvar 'w)
        c (fd/+c u v w)
        s ((addcg c) empty-s)]
    (is (= (count (:km (:cs s))) 2))
    (is (= (count (:cm (:cs s))) 1))))

(deftest test-purge-c
  (let [u (lvar 'u)
        v 1
        w (lvar 'w)
        c (fd/+c u v w)
        s ((addcg c) empty-s)
        c (first (constraints-for (:cs s) s u ::l/fd))
        s (-> s
            (ext-run-cs u 1)
            (ext-run-cs w 2))]
    (is (zero? (count (:km (:cs s)))))
    (is (zero? (count (:cm (:cs s)))))))

(deftest test-=fd-1
  (let [x (lvar 'x)
        y (lvar 'y)
        s ((composeg
            (fd/dom x (fd/interval 1 6))
            (fd/dom y (fd/interval 5 10))) empty-s)
        s ((fd/== x y) s)
        cs (:cs s)]
    (is (= 2 (count (:km (:cs s))))) ;; works
    (is (= 3 (count (:cm (:cs s)))))
    (is (= (fd/get-dom s x) (fd/interval 5 6)))
    (is (= (fd/get-dom s y) (fd/interval 5 6)))))

(deftest test-multi-interval-1
  (let [mi (fd/multi-interval (fd/interval 1 3) (fd/interval 7 10))]
    (is (= 1 (fd/lb mi)))
    (is (= 10 (fd/ub mi)))))

(deftest test-run-constraints*
  (is (= (run-constraints* [] [] ::l/subst) s#)))

(deftest test-drop-one-1
  (is (= (:s (fd/drop-one (fd/domain 1 2 3)))
         #{2 3})))

(deftest test-drop-one-2
  (is (= (fd/drop-one (fd/domain 1))
         nil)))

(deftest test-drop-one-3
  (is (= (fd/drop-one 1)
         nil)))

(deftest test-drop-one-4
  (is (= (fd/drop-one (fd/interval 1 10))
         (fd/interval 2 10))))

(deftest test-drop-one-5
  (is (= (fd/drop-one (fd/interval 1 1))
         nil)))

(deftest test-drop-one-6
  (is (= (fd/drop-one (fd/multi-interval (fd/interval 1 10) (fd/interval 15 20)))
         (fd/multi-interval (fd/interval 2 10) (fd/interval 15 20)))))

(deftest test-to-vals-1
  (is (= (fd/to-vals 1) '(1))))

(deftest test-to-vals-2
  (is (= (fd/to-vals (fd/domain 1 2 3)) '(1 2 3))))

(deftest test-to-vals-3
  (is (= (fd/to-vals (fd/interval 1 10))
         '(1 2 3 4 5 6 7 8 9 10))))

(deftest test-to-vals-4
  (is (= (fd/to-vals (fd/multi-interval (fd/interval 1 5) (fd/interval 7 10)))
         '(1 2 3 4 5 7 8 9 10))))

(deftest test-to-vals-5
  (is (= (fd/to-vals (fd/multi-interval (fd/interval 1 5) 7 (fd/interval 9 12)))
         '(1 2 3 4 5 7 9 10 11 12))))

(deftest test-map-sum-1
  (let [x (lvar 'x)
        s ((fd/dom x (fd/interval 1 10)) empty-s)]
    (is (= (take 10
             (solutions s x
               ((fd/map-sum (fn [v] (ext-run-csg x v)))
                (fd/to-vals (fd/interval 1 10)))))
           '(1 2 3 4 5 6 7 8 9 10)))))

(deftest test-force-ans-1
  (let [x (lvar 'x)
        s ((fd/dom x (fd/interval 1 10)) empty-s)]
    (is (= (take 10
             (solutions s x
               (force-ans x)))
           '(1 2 3 4 5 6 7 8 9 10)))))

(deftest test-force-ans-2
  (let [x (lvar 'x)
        s ((fd/dom x (fd/interval 1 10)) empty-s)]
    (is (= (take 10
             (solutions s x
               (force-ans [x])))
           '(1 2 3 4 5 6 7 8 9 10)))))

(deftest test-force-ans-3
  (let [x (lvar 'x)
        s ((fd/dom x (fd/multi-interval (fd/interval 1 4) (fd/interval 6 10)))
            empty-s)]
    (is (= (take 10
             (solutions s x
               (force-ans x)))
           '(1 2 3 4 6 7 8 9 10)))))

(deftest test-verify-all-bound-1
  (let [x (lvar 'x)
        y (lvar 'y)
        s ((composeg
            (fd/dom x (fd/interval 1 10))
            (fd/dom y (fd/interval 1 10))) empty-s)]
    (is (nil? (verify-all-bound s [x y])))))

(deftest test-verify-all-bound-2
  (let [x (lvar 'x)
        y (lvar 'y)
        s ((fd/dom x (fd/interval 1 10)) empty-s)]
    (is (thrown? Exception (verify-all-bound s [x y])))))

(deftest test-enforce-constraints-1
  (let [x (lvar 'x)
        s ((fd/dom x (fd/interval 1 3)) empty-s)]
    (is (= (solutions s x
             (enforce-constraints x))
           '(1 2 3)))))

(deftest test-reifyg-1
  (let [x (lvar 'x)
        y (lvar 'y)
        s ((composeg
            (fd/dom x (fd/interval 1 10))
            (fd/dom y (fd/interval 1 5))) empty-s)
        s ((fd/== x y) s)]
    (is (= (take* ((reifyg x) s))
           '(1 2 3 4 5)))))

(deftest test-process-interval-smaller-1
  (let [x (lvar 'x)
        s ((composeg
            (fd/dom x (fd/interval 1 10))
            (fd/dom x (fd/interval 2 10))) empty-s)]
    (is (= (fd/get-dom s x)
           (fd/interval 2 10)))))

(deftest test-boundary-interval-1
  (is (fd/difference (fd/interval 1 10) 1)
      (fd/interval 2 10)))

(deftest test-boundary-interval-1
  (is (fd/difference (fd/interval 1 10) 10)
      (fd/interval 1 9)))

(deftest test-process-imi-1
  (let [x (lvar 'x)
        s ((composeg
            (fd/dom x (fd/interval 2 10))
            (fd/dom x (fd/multi-interval (fd/interval 1 4) (fd/interval 6 10))))
           empty-s)]
    (is (= (fd/get-dom s x)
           (fd/multi-interval (fd/interval 2 4) (fd/interval 6 10))))))

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
  (is (= (into #{}
           (run* [q]
             (fresh [x]
               (fd/in x (fd/interval 1 3))
               (== q x))))
         (into #{} '(1 2 3)))))

(deftest test-ckanren-2
  (is (= (into #{}
           (run* [q]
             (fresh [x y z]
               (fd/in x z (fd/interval 1 5))
               (fd/in y (fd/interval 3 5))
               (fd/+ x y z)
               (== q [x y z]))))
         (into #{} '([1 3 4] [2 3 5] [1 4 5])))))

(deftest test-ckanren-3
  (is (= (into #{}
           (run* [q]
             (fresh [x y]
               (fd/in x y (fd/interval 1 3))
               (fd/== x y)
               (== q [x y]))))
         (into #{} '([1 1] [2 2] [3 3])))))

(deftest test-ckanren-4
  (is (true?
       (every? (fn [[x y]] (not= x y))
         (run* [q]
           (fresh [x y]
             (fd/in x y (fd/interval 1 10))
             (fd/!= x y)
             (== q [x y])))))))

(deftest test-ckanren-5
  (is (= (into #{}
           (run* [q]
             (fresh [x y]
               (fd/in x y (fd/interval 1 3))
               (== x 2)
               (fd/!= x y)
               (== q [x y]))))
         (into #{} '([2 1] [2 3])))))

(deftest test-ckanren-6
  (is (= (run* [q]
           (fresh [x]
             (fd/in x (fd/interval 1 3))
             (fd/+ x 1 x)
             (== q x)))
         '())))

(deftest test-ckanren-7
  (is (= (run* [q]
           (fresh [x]
             (fd/in x (fd/interval 1 3))
             (fd/+ x x x)))
         '())))

(deftest test-ckanren-8
  (is (= (into #{}
           (run* [q]
             (fresh [x y]
               (fd/in x y (fd/interval 1 3))
               (fd/<= x y)
               (== q [x y]))))
         (into #{} '([1 1] [1 2] [2 2] [1 3] [3 3] [2 3])))))

(deftest test-ckanren-9
  (is (= (into #{}
           (run* [q]
             (fresh [x y]
               (fd/in x y (fd/interval 1 3))
               (fd/< x y)
               (== q [x y]))))
         (into #{} '([1 2] [2 3] [1 3])))))

(defn subgoal [x]
  (fresh [y]
    (== y x)
    (fd/+ 1 y 3)))

(deftest test-ckanren-10
  (is (= (run* [q]
           (fresh [x]
             (fd/in x (fd/interval 1 10))
             (subgoal x)
             (== q x)))
         '(2))))

(deftest test-list-sorted
  (is (true? (fd/list-sorted? < [1 2 3])))
  (is (true? (fd/list-sorted? < [1 3 5])))
  (is (false? (fd/list-sorted? < [1 1 3])))
  (is (false? (fd/list-sorted? < [1 5 4 1]))))

(deftest test-with-id
  (let [x (lvar 'x)
        y (lvar 'y)
        n* (sorted-set 1 3 5)
        c (with-id (fd/-distinctc x #{y} (conj n* 7)) 1)]
    (is (= (id c) 1))))

(deftest test-distinct
  (is (= (into #{}
           (run* [q]
             (fresh [x y z]
               (fd/in x y z (fd/interval 1 3))
               (fd/distinct [x y z])
               (== q [x y z]))))
         (into #{} '([1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1])))))

(deftest test-=fd-1
  (is (= (into #{}
           (run* [q]
             (fresh [a b]
               (fd/in a b (fd/interval 1 3))
               (fd/== a b)
               (== q [a b]))))
         (into #{} '([1 1] [2 2] [3 3])))))

(deftest test-fd-!=-1
  (is (= (into #{}
           (run* [q]
             (fresh [a b]
               (fd/in a b (fd/interval 1 3))
               (fd/!= a b)
               (== q [a b]))))
         (into #{} '([1 2] [1 3] [2 1] [2 3] [3 1] [3 2])))))

(deftest test-fd-<-1
  (is (= (into #{}
           (run* [q]
             (fresh [a b c]
               (fd/in a b c (fd/interval 1 3))
               (fd/< a b) (fd/< b c)
               (== q [a b c]))))
         (into #{} '([1 2 3])))))

(deftest test-fd-<-2
  (is (= (into #{}
           (run* [q]
             (fresh [x y z]
               (fd/in x y z (fd/interval 1 10))
               (fd/+ x y z)
               (fd/< x y)
               (== z 10)
               (== q [x y z]))))
         (into #{} '([1 9 10] [2 8 10] [3 7 10] [4 6 10])))))

(deftest test-fd->-1
  (is (= (into #{}
           (run* [q]
             (fresh [x y z]
               (fd/in x y z (fd/interval 1 10))
               (fd/+ x y z)
               (fd/> x y)
               (== z 10)
               (== q [x y z]))))
         (into #{} '([6 4 10] [7 3 10] [8 2 10] [9 1 10])))))

(deftest test-fd-<=-1
  (is (= (run* [q]
           (fresh [x y]
             (== x 3)
             (fd/in y (fd/multi-interval 2 4))
             (fd/<= x y)
             (== q y)))
         '(4))))

(deftest test-fd->=-1
  (is (= (run* [q]
           (fresh [x y]
             (== x 3)
             (fd/in y (fd/multi-interval 2 4))
             (fd/>= x y)
             (== q y)))
         '(2))))

(deftest test-fd-*-1
  (is (= (into #{}
           (run* [q]
             (fresh [n m]
               (fd/in n m (fd/interval 1 10))
               (fd/* n 2 m)
               (== q [n m]))))
         (into #{} '([1 2] [2 4] [3 6] [4 8] [5 10])))))

(deftest test-fd-*-2
  (is (= (into #{}
           (run* [q]
             (fresh [n m]
               (fd/in n m (fd/interval 1 10))
               (fd/* n m 10)
               (== q [n m]))))
         (into #{} '([1 10] [2 5] [5 2] [10 1])))))

;; -----------------------------------------------------------------------------
;; CLP(Tree)

#_(deftest test-recover-vars []
  (let [x (lvar 'x)
        y (lvar 'y)
        s (-> empty-s
              (ext-no-check x 1)
              (ext-no-check y 2))]
    (is (= (recover-vars (:l s))
           #{x y}))))

#_(deftest test-prefix-s []
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

#_(deftest test-prefix-subsumes? []
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
        c (fd/+c x y z)
        cs (addc (make-cs) empty-s c)
        cp (get (:cm cs) 0)
        cs (remc cs empty-s cp)]
    (is (= (:km cs) {}))
    (is (= (:cm cs) {}))))

(deftest test-treec-id-1 []
  (let [x (lvar 'x)
        y (lvar 'y)
        c (with-id (!= x y) 0)]
    (is (zero? (id c)))))

(deftest test-tree-constraint? []
  (let [x (lvar 'x)
        y (lvar 'y)
        c (!=c (list (pair x 1) (pair y 2)))
        cs (addc (make-cs) empty-s c)]
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
    (is (= (prefix ((:cm (:cs s)) 0)) {x y}))))

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
    (is (= (prefix ((:cm (:cs s)) 0)) {x 1}))))

#_(deftest test-normalize-store []
  (let [x (lvar 'x)
        y (lvar 'y)
        c (!=c (list (pair x 1)))
        sc (!=c (list (pair x 1) (pair y 2)))
        cs (addc (make-cs) empty-s c)]
    ))

(deftest test-multi-constraints-1 []
  (is (= (run* [q]
           (fresh [x y z]
             (fd/in x y z (fd/interval 1 3))
             (!= z 3)
             (fd/+ x y z)
             (== q [x y z])))
         '([1 1 2]))))

(deftest test--fd-1 []
  (is (= (run* [q]
           (fd/in q (fd/interval 1 10))
           (fd/- 4 q 1))
         '(3)))
  (is (= (run* [q]
           (fd/in q (fd/interval 1 10))
           (fd/- 4 2 q))
         '(2))))

(deftest test-quot-1 []
  (is (= (run* [q]
           (fd/in q (fd/interval 1 10))
           (fd/quot 4 2 q))
         '(2))))

;; =============================================================================
;; fd/eq

(deftest test-fd-eq-1 []
  (is (= (run* [q]
           (fresh [x y]
             (fd/in x y (fd/interval 0 9))
             (fd/eq
              (= (+ x y) 9)
              (= (+ (* x 2) (* y 4)) 24))
             (== q [x y])))
         '([6 3]))))

(deftest test-fd-eq-2 []
  (is (= (run* [q]
           (fresh [s e n d m o r y]
             (== q [s e n d m o r y])
             (fd/in s e n d m o r y (fd/interval 0 9))
             (fd/distinct [s e n d m o r y])
             (fd/!= m 0) (fd/!= s 0)
             (fd/eq
              (= (+ (* 1000 s) (* 100 e) (* 10 n) d
                    (* 1000 m) (* 100 o) (* 10 r) e)
                 (+ (* 10000 m) (* 1000 o) (* 100 n) (* 10 e) y)))))
         '([9 5 6 7 1 0 8 2]))))

(deftest test-fd-eq-3 []
  (is (= (run* [q]
           (fresh [x y]
             (fd/in x y (fd/interval 1 20))
             (fd/eq
              (= (+ x y) 11)
              (= (- (* 3 x) y) 5))
             (== q [x y])))
         '([4 7]))))

(deftest test-fd-distinct-1 []
  (is (= (run 1 [q]
           (fresh [x y]
             (fd/distinct q)
             (== q [x y])
             (== x 1)
             (== y 1)))
         ())))

(deftest test-logic-62-fd []
  (is (= (run 1 [q]
           (fresh [x y a b]
             (fd/distinct [x y])
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
             (fd/distinct [x y])))
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

(deftest test-logic-81-fd []
  (is (= (run* [q]
           (fresh [x y]
             (== q x)
             (fd/distinct [q y])
             (== y x)
             (fd/in q x y (fd/interval 1 3))))
        ()))
  (is (= (run* [q]
           (fresh [x y z]
             (== q x)
             (== y z)
             (fd/distinct [q y])
             (fd/distinct [q x])
             (== z q)
             (fd/in q x y z (fd/interval 1 3))))
        ())))

;; =============================================================================
;; predc

(deftest test-predc-1 []
  (is (= (run* [q]
           (predc q number? `number?))
         '((_0 :- clojure.core/number?))))
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
        ()))
  (is (= (run* [q]
           (fresh [x]
             (predc q number? `number?)
             (== q x)
             (== x "foo")))
        ()))
  (is (= (run* [q]
           (fresh [x]
             (== q x)
             (predc q number? `number?)
             (== x "foo")))
        ())))

(deftest test-predc-custom-reify-1
  (is (= (run* [q]
           (predc q number? (fn [c v r a] `(~'num ~(walk* r (walk* a q))))))
        '((_0 :- (num _0)))))
  (is (= (run* [q]
           (fresh [x y]
             (predc x number? (fn [c v r a] `(~'num ~(walk* r (walk* a x)))))
             (== [x y] q)))
        '(([_0 _1] :- (num _0)))))
  (is (= (run* [q]
           (predc q number? (fn [c v r a] nil)))
        '(_0))))

;; =============================================================================
;; Real cKanren programs

(defn not-adjacento [x y]
  (fresh [f]
    (fd/in f (fd/interval 1 5))
    (conde
      [(fd/+ x f y) (fd/< 1 f)]
      [(fd/+ y f x) (fd/< 1 f)])))

(defn dinesmanfd []
  (run* [baker cooper fletcher miller smith :as vs]
    (fd/distinct vs)
    (everyg #(fd/in % (fd/interval 1 5)) vs)
    (fd/!= baker 5) (fd/!= cooper 1)
    (fd/!= fletcher 5) (fd/!= fletcher 1)
    (fd/< cooper miller) 
    (not-adjacento smith fletcher)
    (not-adjacento fletcher cooper)))

(deftest test-dinesmandfd []
  (is (= (dinesmanfd) '([3 2 4 5 1]))))

(defne subchecko [w sl r o n]
  ([_ () _ _ _]
     (fresh [hr]
       (fd/in hr (fd/interval 1 n))
       (matche [r o]
         ([[hr . _] [w . r]] (fd/+ hr 1 w))
         ([() [w . r]]))))
  ([_ [hsl . rsl] _ _ _]
     (fresh [w-hsl w+hsl o0 o1 nw]
       (fd/in hsl w-hsl w+hsl (fd/interval 1 n))
       (fd/+ hsl w-hsl w) (fd/+ hsl w w+hsl)
       (subchecko w-hsl rsl r  o0 n)
       (subchecko w     rsl o0 o1 n)
       (subchecko w+hsl rsl o1 o  n))))

(defne checko [ws sl r n]
  ([() _ [a . _] a])
  ([[w . wr] _ _ _]
     (fresh [nsl nr]
       (subchecko w sl r nr n)
       (conso w sl nsl)
       (checko wr nsl nr n))))

(defn matches [n]
  (run 1 [a b c d]
    (fd/in a b c d (fd/interval 1 n)) 
    (fd/distinct [a b c d])
    (== a 1)
    (fd/<= a b) (fd/<= b c) (fd/<= c d)
    (fd/eq (= (+ a b c d) n))
    (checko [a b c d] () () n)))

(deftest test-matches
  (is (= (matches 40) '([1 3 9 27]))))

(defn get-square [rows x y]
  (for [x (range x (+ x 3))
        y (range y (+ y 3))]
    (get-in rows [x y])))

(defn init [vars hints]
  (if (seq vars)
    (let [hint (first hints)]
      (all
       (if-not (zero? hint)
         (== (first vars) hint)
         succeed)
       (init (next vars) (next hints))))
    succeed))

(defn ->rows [xs]
  (->> xs (partition 9) (map vec) (into [])))

(defn ->cols [rows]
  (apply map vector rows))

(defn ->squares [rows]
  (for [x (range 0 9 3)
        y (range 0 9 3)]
    (get-square rows x y)))

(defn sudokufd [hints]
  (let [vars (repeatedly 81 lvar) 
        rows (->rows vars)
        cols (->cols rows)
        sqs  (->squares rows)]
    (run-nc 1 [q]
      (== q vars)
      (distribute q ::l/ff)
      (everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) vars)
      (init vars hints)
      (everyg fd/distinct rows)
      (everyg fd/distinct cols)
      (everyg fd/distinct sqs))))

(defn verify [vars]
  (let [rows (->rows vars)
        cols (->cols rows)
        sqs  (->squares rows)
        verify-group (fn [group]
                       (every? #(= (->> % (into #{}) count) 9)
                          group))]
    (and (verify-group rows)
         (verify-group cols)
         (verify-group sqs))))

(def easy0
    [0 0 3  0 2 0  6 0 0
     9 0 0  3 0 5  0 0 1
     0 0 1  8 0 6  4 0 0

     0 0 8  1 0 2  9 0 0
     7 0 0  0 0 0  0 0 8
     0 0 6  7 0 8  2 0 0

     0 0 2  6 0 9  5 0 0
     8 0 0  2 0 3  0 0 9
     0 0 5  0 1 0  3 0 0])

(deftest test-sudokufd
  (is (-> (sudokufd easy0) first verify)))

(defn safefd []
  (run* [c1 c2 c3 c4 c5 c6 c7 c8 c9 :as vs]
    (everyg #(fd/in % (fd/interval 1 9)) vs)
    (fd/distinct vs)
    (fd/eq
     (= (- c4 c6) c7)
     (= (* c1 c2 c3) (+ c8 c9))
     (< (+ c2 c3 c6) c8)
     (< c9 c8))
    (project [vs]
      (everyg (fn [[v n]] (fd/!= v n))
        (map vector vs (range 1 10))))))

(deftest test-safefd
  (is (= (safefd)
         '([4 3 1 8 9 2 6 7 5]))))

;; =============================================================================
;; Feature Constraints

(deftest test-featurec-1
  (is (= (run* [q]
           (featurec q {:foo 1}))
         '((_0 :- (clojure.core.logic/featurec _0 {:foo 1})))))
  (is (= (run* [q]
           (featurec q {:foo 1})
           (== q {:foo 1 :bar 2}))
         '({:foo 1 :bar 2})))
  (is (= (run* [q]
           (featurec q {:foo 1})
           (== q {:bar 2}))
         ()))
  (is (= (run* [q]
           (fresh [x]
             (featurec x {:foo q})
             (== x {:foo 1})))
         '(1))))

;; =============================================================================
;; Deep Constraints

(deftest test-treec-1
  (is (= (run* [q]
           (treec q #(predc % number?) `number?)
           (fresh [x y]
             (== q [x [2 3 y]])
             (== x 1)))
         '(([1 [2 3 _0]] :- (clojure.core.logic/fixc _0 clojure.core/number?)))))
  (is (= (run* [q]
           (treec q #(predc % number?) `number?)
           (fresh [x y]
             (== q [x [2 3 y]])
             (== x 1)
             (== y 'foo)))
         ()))
  (is (= (run* [q]
           (treec q #(predc % number?) `number?)
           (fresh [z]
             (== q {:x {:y z}})))
         '(({:x {:y _0}} :- (clojure.core.logic/fixc _0 clojure.core/number?)))))
  (is (= (run* [q]
           (treec q #(predc % number?) `number?)
           (fresh [z]
             (== q {:x {:y z}})
             (== z 1)))
         '({:x {:y 1}})))
  (is (= (run* [q]
           (treec q #(predc % number?) `number?)
           (fresh [z]
             (== q {:x {:y z}})
             (== z 'foo)))
         ()))
  (is (= (run* [q]
           (treec q #(predc % number?) `number?)
           (fresh [x]
             (== q (llist 1 2 x))))
         [[(llist 1 2 '_0) ':- '(clojure.core.logic/fixc _0 clojure.core/number?)]]))
  (is (= (run* [q]
           (treec q #(predc % number?) `number?)
           (fresh [x]
             (== q (llist 1 2 x))
             (== x '(3))))
         '((1 2 3))))
  (is (= (run* [q]
           (treec q #(predc % number?) `number?)
           (fresh [x]
             (== q (llist 1 2 x))
             (== x '(foo))))
         ())))

(deftest test-treec-custom-reify-1
  (is (= (run* [q]
           (fresh [x]
             (treec q #(predc % number?)
               (fn [c _ v r a]
                 `(~'hashc ~v ~(-reify a x r))))))
         '((_0 :- (hashc _0 _1))))))

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

(deftest test-ext-run-cs-1 []
  (let [x (lvar 'x)
        s (ext-no-check empty-s x (subst-val ::l/unbound))
        s (add-attr s x ::l/fd (fd/domain 1 2 3))
        s (ext-run-cs s x 1)]
    (is (= (root-val s x) 1))
    (is (= (walk s x) 1))))

(deftest test-update-dom-1 []
  (let [x (lvar 'x)
        s (add-dom empty-s x ::nom '[(swap a b)])
        s (update-dom s x ::nom (fn [d] (conj d '(swap x y))))]
    (is (= (get-dom s x ::nom) '[(swap a b) (swap x y)]))))

(deftest test-update-dom-2 []
  (let [x (lvar 'x)
        s (update-dom empty-s x ::nom (fnil (fn [d] (conj d '(swap x y))) []))]
    (is (= (get-dom s x ::nom) '[(swap x y)]))))
