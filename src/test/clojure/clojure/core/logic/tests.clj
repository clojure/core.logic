(ns clojure.core.logic.tests
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is]] :reload)
  (:use clojure.test))

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

(defn rembero [x l out]
  (conde
    [(== '() l) (== '() out)]
    [(fresh [a d]
       (conso a d l)
       (== x a)
       (== d out))]
    [(fresh [a d res]
       (conso a d l)
       (conso a res out)
       (rembero x d res))]))

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
             (conso a d '())
             (== (cons a d) q))
           []))))

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

(defn onceo [g]
  (condu
    (g s#)))

(deftest test-condu-1
  (is (= (run* [x]
           (onceo (teacupo x)))
         '(tea))))

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
         '(_.0))))

(deftest test-disequality-2
  (is (= (run* [q]
           (fresh [x]
             (== q x)
             (!= x 1)))
         '(_.0))))

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
;; Tickets

(deftest test-32-unify-set
  (is (= (run* [q] (== q #{1}))
         '(#{1}))))

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
