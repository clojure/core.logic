(ns cljs.core.logic.tests
  (:refer-clojure :exclude [==])
  (:require-macros [clojure.tools.macro :as mu])
  (:require
   [cljs.test :as test :refer-macros [deftest is testing run-tests]]
   [cljs.core.logic :as l
    :refer [pair lvar lcons -unify -ext-no-check -walk -walk*
            -reify-lvar-name empty-s to-s succeed fail s# u# conso
            nilo firsto resto emptyo appendo membero
            unifier binding-map partial-map failed?]
    :refer-macros [run run* run-nc == conde conda condu fresh defne matche all]]
   [cljs.core.logic.pldb :as pldb :include-macros true]))

(defn js-print [& args]
  (if (exists? js/console)
    (.log js/console (apply str args))
    (js/print (apply str args))))

(set! *print-fn* js-print)

;; =============================================================================
;; unify

;; -----------------------------------------------------------------------------
;; unify with nil

(deftest test-unify-with-nil
  (testing "Unify with nil"
    (let [x (lvar 'x)]
      (is (= (pair x nil) (pair x nil))))
    (let [x (lvar 'x)]
      (is (false? (= (pair x nil) (pair nil x)))))
    (is (failed? (-unify empty-s nil 1)))
    (let [x (lvar 'x)
          a (-ext-no-check empty-s x nil)
          b (-unify empty-s nil x)]
      (is (= a b)))
    (let [x (lvar 'x)]
      (is (failed? (-unify empty-s nil (lcons 1 x)))))
    (let [x (lvar 'x)]
      (is (failed? (-unify empty-s nil {}))))
    (let [x (lvar 'x)]
      (is (failed? (-unify empty-s nil #{}))))))

;; -----------------------------------------------------------------------------
;; unify with object

(deftest test-unify-with-object
  (testing "Unify wiht object"
    (is (failed? (-unify empty-s 1 nil)))
    (is (= (-unify empty-s 1 1) empty-s))
    (is (= (-unify empty-s :foo :foo) empty-s))
    (is (= (-unify empty-s 'foo 'foo) empty-s))
    (is (= (-unify empty-s "foo" "foo") empty-s))
    (is (failed? (-unify empty-s 1 2)))
    (is (failed? (-unify empty-s 2 1)))
    (is (failed? (-unify empty-s :foo :bar)))
    (is (failed? (-unify empty-s 'foo 'bar)))
    (is (failed? (-unify empty-s "foo" "bar")))
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x 1)]
      (is (= (-unify empty-s 1 x) os)))
    (let [x (lvar 'x)]
      (is (failed? (-unify empty-s 1 (lcons 1 'x)))))
    (is (failed? (-unify empty-s 1 '())))
    (is (failed? (-unify empty-s 1 '[])))
    (is (failed? (-unify empty-s 1 {})))
    (is (failed? (-unify empty-s 1 #{})))))

;; -----------------------------------------------------------------------------
;; unify with lvar

(deftest test-unify-with-lvar
  (testing "Unify with lvar"
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x 1)]
      (is (= (-unify empty-s x 1) os)))
    (let [x (lvar 'x)
          y (lvar 'y)
          os (-ext-no-check empty-s x y)]
      (is (= (-unify empty-s x y) os)))
    (let [x (lvar 'x)
          y (lvar 'y)
          l (lcons 1 y)
          os (-ext-no-check empty-s x l)]
      (is (= (-unify empty-s x l) os)))
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x [])]
      (is (= (-unify empty-s x []) os)))
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x [1 2 3])]
      (is (= (-unify empty-s x [1 2 3]) os)))
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x '())]
      (is (= (-unify empty-s x '()) os)))
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x '(1 2 3))]
      (is (= (-unify empty-s x '(1 2 3)) os)))
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x {})]
      (is (= (-unify empty-s x {}) os)))
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x {1 2 3 4})]
      (is (= (-unify empty-s x {1 2 3 4}) os)))
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x #{})]
      (is (= (-unify empty-s x #{}) os)))
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x #{1 2 3})]
      (is (= (-unify empty-s x #{1 2 3}) os))))
  )

;; -----------------------------------------------------------------------------
;; unify with lcons

(deftest test-unify-with-lcons
  (testing "Unify with lcons"
    (let [x (lvar 'x)]
      (is (failed? (-unify empty-s (lcons 1 x) 1))))
    (let [x (lvar 'x)
          y (lvar 'y)
          l (lcons 1 y)
          os (-ext-no-check empty-s x l)]
      (is (= (-unify empty-s l x) os)))
    (let [x (lvar 'x)
          y (lvar 'y)
          lc1 (lcons 1 x)
          lc2 (lcons 1 y)
          os (-ext-no-check empty-s x y)]
      (is (= (-unify empty-s lc1 lc2) os)))

    ;; NOTE: sketchy tests that makes ordering assumptions about representation
    ;; - David

    ;; START HERE
    (let [x (lvar 'x)
          y (lvar 'y)
          z (lvar 'z)
          lc1 (lcons 1 (lcons 2 x))
          lc2 (lcons 1 (lcons z y))
          os (-> empty-s
               (-ext-no-check z 2)
               (-ext-no-check x y))]
      (is (= (-unify empty-s lc1 lc2) os)))
    (let [x (lvar 'x)
          y (lvar 'y)
          lc1 (lcons 1 (lcons 2 x))
          lc2 (lcons 1 (lcons 2 (lcons 3 y)))
          os (-ext-no-check empty-s x (lcons 3 y))]
      (is (= (-unify empty-s lc1 lc2) os)))
    (let [x (lvar 'x)
          y (lvar 'y)
          lc1 (lcons 1 (lcons 2 x))
          lc2 (lcons 1 (lcons 3 (lcons 4 y)))]
      (is (failed? (-unify empty-s lc1 lc2))))
    (let [x (lvar 'x)
          y (lvar 'y)
          lc2 (lcons 1 (lcons 2 x))
          lc1 (lcons 1 (lcons 3 (lcons 4 y)))]
      (is (failed? (-unify empty-s lc1 lc2))))
    (let [x (lvar 'x)
          y (lvar 'y)
          lc1 (lcons 1 (lcons 2 x))
          lc2 (lcons 1 (lcons 2 y))
          os (-ext-no-check empty-s x y)]
      (is (= (-unify empty-s lc1 lc2) os)))
    (let [x (lvar 'x)
          lc1 (lcons 1 (lcons 2 x))
          l1 '(1 2 3 4)
          os (-ext-no-check empty-s x '(3 4))]
      (is (= (-unify empty-s lc1 l1) os)))
    (let [x (lvar 'x)
          y (lvar 'y)
          lc1 (lcons 1 (lcons y (lcons 3 x)))
          l1 '(1 2 3 4)
          os (-> empty-s
               (-ext-no-check y 2)
               (-ext-no-check x '(4)))]
      (is (= (-unify empty-s lc1 l1) os)))
    (let [x (lvar 'x)
          lc1 (lcons 1 (lcons 2 (lcons 3 x)))
          l1 '(1 2 3)
          os (-ext-no-check empty-s x '())]
      (is (= (-unify empty-s lc1 l1) os)))
    (let [x (lvar 'x)
          lc1 (lcons 1 (lcons 3 x))
          l1 '(1 2 3 4)]
      (is (failed? (-unify empty-s lc1 l1))))
    (let [x (lvar 'x)
          lc1 (lcons 1 (lcons 2 x))
          l1 '(1 3 4 5)]
      (is (failed? (-unify empty-s lc1 l1))))
    (is (failed? (-unify empty-s (lcons 1 (lvar 'x)) {})))
    (is (failed? (-unify empty-s (lcons 1 (lvar 'x)) #{}))))
  )

;; -----------------------------------------------------------------------------
;; unify with sequential

(deftest test-unify-with-sequential
  (testing "Unify wiht sequential"
    (is (failed? (-unify empty-s '() 1)))
    (is (failed? (-unify empty-s [] 1)))
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x [])]
      (is (= (-unify empty-s [] x) os)))
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x [])]
      (is (= (-unify empty-s [] x) os)))
    (let [x (lvar 'x)
          lc1 (lcons 1 (lcons 2 x))
          l1 '(1 2 3 4)
          os (-ext-no-check empty-s x '(3 4))]
      (is (= (-unify empty-s l1 lc1) os)))
    (is (= (-unify empty-s [1 2 3] [1 2 3]) empty-s))
    (is (= (-unify empty-s '(1 2 3) [1 2 3]) empty-s))
    (is (= (-unify empty-s '(1 2 3) '(1 2 3)) empty-s))
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x 2)]
      (is (= (-unify empty-s `(1 ~x 3) `(1 2 3)) os)))
    (is (failed? (-unify empty-s [1 2] [1 2 3])))
    (is (failed? (-unify empty-s '(1 2) [1 2 3])))
    (is (failed? (-unify empty-s [1 2 3] [3 2 1])))
    (is (= (-unify empty-s '() '()) empty-s))
    (is (failed? (-unify empty-s '() '(1))))
    (is (failed? (-unify empty-s '(1) '())))
    (is (= (-unify empty-s [[1 2]] [[1 2]]) empty-s))
    (is (failed? (-unify empty-s [[1 2]] [[2 1]])))
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x 1)]
      (is (= (-unify empty-s [[x 2]] [[1 2]]) os)))
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x [1 2])]
      (is (= (-unify empty-s [x] [[1 2]]) os)))
    (let [x (lvar 'x) y (lvar 'y)
          u (lvar 'u) v (lvar 'v)
          os (-> empty-s
               (-ext-no-check y 'a)
               (-ext-no-check x 'b))]
      (is (= (-unify empty-s ['a x] [y 'b]) os)))
    (is (failed? (-unify empty-s [] {})))
    (is (failed? (-unify empty-s '() {})))
    (is (failed? (-unify empty-s [] #{})))
    (is (failed? (-unify empty-s '() #{}))))
  )

;; -----------------------------------------------------------------------------
;; unify with map

(deftest test-unify-with-map
  (testing "Unify wiht map"
    (is (failed? (-unify empty-s {} 1)))
    (let [x (lvar 'x)
          os (-ext-no-check empty-s x {})]
      (is (= (-unify empty-s {} x) os)))
    (let [x (lvar 'x)]
      (is (failed? (-unify empty-s {} (lcons 1 x)))))
    (is (failed? (-unify empty-s {} '())))
    (is (= (-unify empty-s {} {}) empty-s))
    (is (= (-unify empty-s {1 2 3 4} {1 2 3 4}) empty-s))
    (is (failed? (-unify empty-s {1 2} {1 2 3 4})))
    (let [x (lvar 'x)
          m1 {1 2 3 4}
          m2 {1 2 3 x}
          os (-ext-no-check empty-s x 4)]
      (is (= (-unify empty-s m1 m2) os)))
    (let [x (lvar 'x)
          m1 {1 2 3 4}
          m2 {1 4 3 x}]
      (is (failed? (-unify empty-s m1 m2))))
    (is (failed? (-unify empty-s {} #{}))))
  )

;; =============================================================================
;; walk

(deftest test-walk
  (testing "Walk"
    (is (= (let [x (lvar 'x)
                     y (lvar 'y)
                     s (to-s [[x 5] [y x]])]
                 (-walk s y))
              5))

    (is (= (let [[x y z c b a :as s] (map lvar '[x y z c b a])
                     s (to-s [[x 5] [y x] [z y] [c z] [b c] [a b]])]
                 (-walk s a))
              5)))
  )

;; =============================================================================
;; reify

(deftest test-reify
  (is (= (let [x (lvar 'x)
                   y (lvar 'y)]
               (-reify-lvar-name (to-s [[x 5] [y x]])))
            '_.2))
  )

;; =============================================================================
;; walk*

(deftest test-walk*
  (is (= (let [x (lvar 'x)
                   y (lvar 'y)]
               (-walk* (to-s [[x 5] [y x]]) `(~x ~y)))
            '(5 5))))

;; =============================================================================
;; run and unify

(deftest test-run-and-unfiy
  (is (= (run* [q]
               (== true q))
            '(true)))

  (is (= (run* [q]
               (fresh [x y]
                 (== [x y] [1 5])
                 (== [x y] q)))
            [[1 5]]))

  (is (= (run* [q]
               (fresh [x y]
                 (== [x y] q)))
            '[[_.0 _.1]]))
  )

;; =============================================================================
;; fail

(deftest test-fail
  (is (= (run* [q]
               fail
               (== true q))
            [])))

;; =============================================================================
;; Basic

(deftest test-basic
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

(deftest test-conde1
  (is (= (run* [x]
           (conde
             [(== x 'olive) succeed]
             [succeed succeed]
             [(== x 'oil) succeed]))
        '[olive _.0 oil]))

  (is (= (run* [r]
           (fresh [x y]
             (conde
               [(== 'split x) (== 'pea y)]
               [(== 'navy x) (== 'bean y)])
             (== (cons x (cons y ())) r)))
        '[(split pea) (navy bean)]))
  )

(defn teacupo [x]
  (conde
    [(== 'tea x) s#]
    [(== 'cup x) s#]))

(deftest test-conde2
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
  (testing "conso"
    (is (= (run* [q]
             (fresh [a d]
               (conso a d ())
               (== (cons a d) q)))
          []))

    (let [a (lvar 'a)
          d (lvar 'd)]
      (is (= (run* [q]
               (conso a d q))
            [(lcons a d)])))

    (is (= (run* [q]
             (== [q] nil))
          []))

    (is (=
          (run* [q]
            (conso 'a nil q))
          '[(a)]))

    (is (= (run* [q]
             (conso 'a '(d) q))
          '[(a d)]))

    (is (= (run* [q]
             (conso 'a q '(a)))
          '[()]))

    (is (= (run* [q]
             (conso q '(b c) '(a b c)))
          '[a])))
  )

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
        '[(_.0 1 2)]))

  (is (= (run* [q]
           (resto q [1 2]))
        '[(_.0 1 2)]))

  (is (= (run* [q]
           (resto [1 2] q))
        '[(2)]))

  (is (= (run* [q]
           (resto [1 2 3 4 5 6 7 8] q))
        '[(2 3 4 5 6 7 8)]))
  )

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

(deftest test-membero
  (is (= (run* [q]
           (all
             (== q [(lvar)])
             (membero ['foo (lvar)] q)
             (membero [(lvar) 'bar] q)))
        '([[foo bar]])))

  (is (= (run* [q]
           (all
             (== q [(lvar) (lvar)])
             (membero ['foo (lvar)] q)
             (membero [(lvar) 'bar] q)))
        '([[foo bar] _.0] [[foo _.0] [_.1 bar]]
          [[_.0 bar] [foo _.1]] [_.0 [foo bar]])))
  )

;; -----------------------------------------------------------------------------
;; rembero

(deftest rember-o
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

(deftest test-conde-clause-count
  (is (= (run* [q]
           (fresh [x y]
             (digit-1 x)
             (digit-1 y)
             (== q [x y])))
        '([0 0])))

  (is (= (run* [q]
           (fresh [x y]
             (digit-4 x)
             (digit-4 y)
             (== q [x y])))
        '([0 0] [0 1] [0 2] [1 0] [0 3] [1 1] [1 2] [2 0]
          [1 3] [2 1] [3 0] [2 2] [3 1] [2 3] [3 2] [3 3])))
  )

;; -----------------------------------------------------------------------------
;; anyo

(defn anyo [q]
  (conde
    [q s#]
    [(anyo q)]))

(deftest test-anyo
  (is (= (run 1 [q]
           (anyo s#)
           (== true q))
        (list true)))

  (is (= (run 5 [q]
           (anyo s#)
           (== true q))
        (list true true true true true)))
  )

;; -----------------------------------------------------------------------------
;; divergence

(def f1 (fresh [] f1))

(deftest test-divergence1
  (is (= (run 1 [q]
           (conde
             [f1]
             [(== false false)]))
        '(_.0)))

  (is (= (run 1 [q]
           (conde
             [f1 (== false false)]
             [(== false false)]))
        '(_.0)))
  )

(def f2
  (fresh []
    (conde
      [f2 (conde
            [f2]
            [(== false false)])]
      [(== false false)])))

(deftest test-divergence2
  (is (= (run 5 [q] f2)
        '(_.0 _.0 _.0 _.0 _.0))))

;; -----------------------------------------------------------------------------
;; conda (soft-cut)

(deftest test-conda1
  (is (= (run* [x]
           (conda
             [(== 'olive x) s#]
             [(== 'oil x) s#]
             [u#]))
        '(olive)))

  (is (= (run* [x]
           (conda
             [(== 'virgin x) u#]
             [(== 'olive x) s#]
             [(== 'oil x) s#]
             [u#]))
        '()))

  (is (= (run* [x]
           (fresh (x y)
             (== 'split x)
             (== 'pea y)
             (conda
               [(== 'split x) (== x y)]
               [s#]))
           (== true x))
        '()))

  (is (= (run* [x]
           (fresh (x y)
             (== 'split x)
             (== 'pea y)
             (conda
               [(== x y) (== 'split x)]
               [s#]))
           (== true x))
        '(true)))
  )

(defn not-pastao [x]
  (conda
    [(== 'pasta x) u#]
    [s#]))

(deftest test-conda2
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

(deftest test-condu
  (is (= (run* [x]
           (onceo (teacupo x)))
        '(tea)))

  (is (= (run* [r]
           (conde
             [(teacupo r) s#]
             [(== false r) s#]))
        '(false tea cup)))

  (is (= (run* [r]
           (conda
             [(teacupo r) s#]
             [(== false r) s#]))
        '(tea cup)))
  )

;; -----------------------------------------------------------------------------
;; nil in collection

(deftest test-nil-in-coll
  (testing "nil in collection"
    (is (= (run* [q]
             (== q [nil]))
          '([nil])))

    (is (= (run* [q]
             (== q [1 nil]))
          '([1 nil])))

    (is (= (run* [q]
             (== q [nil 1]))
          '([nil 1])))

    (is (= (run* [q]
             (== q '(nil)))
          '((nil))))

    (is (= (run* [q]
             (== q {:foo nil}))
          '({:foo nil})))

    (is (= (run* [q]
             (== q {nil :foo}))
          '({nil :foo}))))
  )

;; -----------------------------------------------------------------------------
;; Unifier

(deftest test-unifier
  (testing "Unifier"
    ;; test-unifier-1
    (is (= (unifier '(?x ?y) '(1 2))
          '(1 2)))
    ;; test-unifier-2
    (is (= (unifier '(?x ?y 3) '(1 2 ?z))
          '(1 2 3)))
    ;; test-unifier-3
    (is (= (unifier '[(?x . ?y) 3] [[1 2] 3])
          '[(1 2) 3]))
    ;; test-unifier-4
    (is (= (unifier '(?x . ?y) '(1 . ?z))
          (lcons 1 '_.0)))
    ;; test-unifier-5
    (is (= (unifier '(?x 2 . ?y) '(1 2 3 4 5))
          '(1 2 3 4 5)))
    ;; test-unifier-6
    (is (= (unifier '(?x 2 . ?y) '(1 9 3 4 5))
          nil))
    ;; test-binding-map-1
    (is (= (binding-map '(?x ?y) '(1 2))
          '{?x 1 ?y 2}))
    ;; test-binding-map-2
    (is (= (binding-map '(?x ?y 3) '(1 2 ?z))
          '{?x 1 ?y 2 ?z 3}))
    ;; test-binding-map-3
    (is (= (binding-map '[(?x . ?y) 3] [[1 2] 3])
          '{?x 1 ?y (2)}))
    ;; test-binding-map-4
    (is (= (binding-map '(?x . ?y) '(1 . ?z))
          '{?z _.0, ?x 1, ?y _.0}))
    ;; test-binding-map-5
    (is (= (binding-map '(?x 2 . ?y) '(1 2 3 4 5))
          '{?x 1 ?y (3 4 5)}))
    ;; test-binding-map-6
    (is (= (binding-map '(?x 2 . ?y) '(1 9 3 4 5))
          nil)))
  )

;; -----------------------------------------------------------------------------
;; Occurs Check

(deftest test-occurs-check
  (is (= (run* [q]
           (== q [q]))
        ())))

;; -----------------------------------------------------------------------------
;; Unifications that should fail

(deftest test-failing-unifications
  (is (= (run* [p]
           (fresh [a b]
             (== b ())
             (== '(0 1) (lcons a b))
             (== p [a b])))
        ()))

  (is (= (run* [p]
           (fresh [a b]
             (== b '(1))
             (== '(0) (lcons a b))
             (== p [a b])))
        ()))

  (is (= (run* [p]
           (fresh [a b c d]
             (== () b)
             (== '(1) d)
             (== (lcons a b) (lcons c d))
             (== p [a b c d])))
        ()))
  )

;; -----------------------------------------------------------------------------
;; Pattern matching other data structures

(defne match-map [m o]
  ([{:foo {:bar o}} _]))

(deftest test-defne1
  (is (= (run* [q]
           (match-map {:foo {:bar 1}} q))
        '(1))))

(defne match-set [s o]
  ([#{:cat :bird :dog} _]))

(deftest tset-defne2
  (is (= (run* [q]
           (match-set #{:cat :bird :dog} q))
        '(_.0))))

;; -----------------------------------------------------------------------------
;; Partial maps

(deftest test-partial-map
  (is (= '({:a 1})
        (run* [q]
          (fresh [pm x]
            (== pm (partial-map {:a x}))
            (== pm {:a 1 :b 2})
            (== pm q)))))

  (is (= '(1)
        (run* [q]
          (fresh [pm x]
            (== pm (partial-map {:a x}))
            (== pm {:a 1 :b 2})
            (== x q)))))
  )

(comment
  ;; FIXME: for some reason set #{:cat :bird} works on match-set call - David
  )

;; =============================================================================
;; zebrao

(defne righto [x y l]
  ([_ _ [x y . r]])
  ([_ _ [_ . r]] (righto x y r)))

(defn nexto [x y l]
  (conde
    [(righto x y l)]
    [(righto y x l)]))

(defn zebrao [hs]
  (mu/symbol-macrolet [_ (lvar)]
   (all
    (== (list _ _ (list _ _ 'milk _ _) _ _) hs)
    (firsto hs (list 'norwegian _ _ _ _))
    (nexto (list 'norwegian _ _ _ _) (list _ _ _ _ 'blue) hs)
    (righto (list _ _ _ _ 'ivory) (list _ _ _ _ 'green) hs)
    (membero (list 'englishman _ _ _ 'red) hs)
    (membero (list _ 'kools _ _ 'yellow) hs)
    (membero (list 'spaniard _ _ 'dog _) hs)
    (membero (list _ _ 'coffee _ 'green) hs)
    (membero (list 'ukrainian _ 'tea _ _) hs)
    (membero (list _ 'lucky-strikes 'oj _ _) hs)
    (membero (list 'japanese 'parliaments _ _ _) hs)
    (membero (list _ 'oldgolds _ 'snails _) hs)
    (nexto (list _ _ _ 'horse _) (list _ 'kools _ _ _) hs)
    (nexto (list _ _ _ 'fox _) (list _ 'chesterfields _ _ _) hs))))

(defn ^:export run_zebra []
  (doall (run-nc 1 [q] (zebrao q))))

(println (pr-str (run 1 [q] (zebrao q))))

(time
  (dotimes [_ 1000]
    (doall (run-nc 1 [q] (zebrao q)))))

(println (pr-str
          (run 10 [q]
            (nexto 'dog 'cat q))))

;; =============================================================================
;; matche

(defn map-geto [m k v]
  (matche [m]
    ([[[k v] . _]])
    ([[_ . tail]] (map-geto tail k v))))

(deftest test-matche
  (is (= (run* [q] (map-geto (seq {:title "Blub"}) :title q)) '("Blub"))))

;; =============================================================================
;; pldb

(pldb/db-rel man p)
(pldb/db-rel woman p)
(pldb/db-rel likes p1 p2)
(pldb/db-rel fun p)

(def ^:dynamic facts0
  (pldb/db
    [man 'Bob]
    [man 'John]
    [man 'Ricky]

    [woman 'Mary]
    [woman 'Martha]
    [woman 'Lucy]

    [likes 'Bob 'Mary]
    [likes 'John 'Martha]
    [likes 'Ricky 'Lucy]))

(def ^:dynamic facts1
  (-> facts0
    (pldb/db-fact fun 'Lucy)))

(deftest test-pldb
  (testing "pldb"
    (pldb/with-db facts0
      (is
        (= (run* [q]
             (fresh [x y]
               (likes x y)
               (fun y)
               (== q [x y])))
          '())))

    (pldb/with-db facts1
      (is
        (= (run* [q]
             (fresh [x y]
               (likes x y)
               (fun y)
               (== q [x y])))
          '([Ricky Lucy])))))
  )

(run-tests)
