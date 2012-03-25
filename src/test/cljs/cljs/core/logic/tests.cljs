(ns cljs.core.logic.tests
  (:refer-clojure :exclude [==])
  (:use-macros
   [clj.core.logic.macros
    :only [run run* == conde conda condu fresh defne matche all]])
  (:require-macros [clj.core.logic.macros :as m])
  (:use
   [cljs.core.logic
    :only [pair lvar lcons -unify -ext-no-check -walk -walk*
           -reify-lvar-name empty-s to-s succeed fail s# u# conso
           nilo firsto resto emptyo appendo membero]]))

(set! *print-fn* js/print)

;; =============================================================================
;; unify

;; -----------------------------------------------------------------------------
;; unify with nil

(let [x (lvar 'x)]
  (assert (= (pair x nil) (pair x nil))))

(let [x (lvar 'x)]
  (assert (false? (= (pair x nil) (pair nil x)))))

(assert (= (-unify empty-s nil 1) false))

(let [x (lvar 'x)
      a (-ext-no-check empty-s x nil)
      b (-unify empty-s nil x)]
  (assert (= a b)))

(let [x (lvar 'x)]
  (assert (= (-unify empty-s nil (lcons 1 x)) false)))

(let [x (lvar 'x)]
  (assert (= (-unify empty-s nil {}) false)))

(let [x (lvar 'x)]
  (assert (= (-unify empty-s nil #{}) false)))

;; -----------------------------------------------------------------------------
;; unify with object

(assert (= (-unify empty-s 1 nil) false))
(assert (= (-unify empty-s 1 1) empty-s))
(assert (= (-unify empty-s :foo :foo) empty-s))
(assert (= (-unify empty-s 'foo 'foo) empty-s))
(assert (= (-unify empty-s "foo" "foo") empty-s))
(assert (= (-unify empty-s 1 2) false))
(assert (= (-unify empty-s 2 1) false))
(assert (= (-unify empty-s :foo :bar) false))
(assert (= (-unify empty-s 'foo 'bar) false))
(assert (= (-unify empty-s "foo" "bar") false))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x 1)]
  (assert (= (-unify empty-s 1 x) os)))

(let [x (lvar 'x)]
  (assert (= (-unify empty-s 1 (lcons 1 'x)) false)))

(assert (= (-unify empty-s 1 '()) false))
(assert (= (-unify empty-s 1 '[]) false))
(assert (= (-unify empty-s 1 {}) false))
(assert (= (-unify empty-s 1 #{}) false))

;; -----------------------------------------------------------------------------
;; unify with lvar

(let [x (lvar 'x)
      os (-ext-no-check empty-s x 1)]
  (assert (= (-unify empty-s x 1) os)))

(let [x (lvar 'x)
      y (lvar 'y)
      os (-ext-no-check empty-s x y)]
  (assert (= (-unify empty-s x y) os)))

(let [x (lvar 'x)
      y (lvar 'y)
      l (lcons 1 y)
      os (-ext-no-check empty-s x l)]
  (assert (= (-unify empty-s x l) os)))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x [])]
  (assert (= (-unify empty-s x []) os)))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x [1 2 3])]
  (assert (= (-unify empty-s x [1 2 3]) os)))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x '())]
  (assert (= (-unify empty-s x '()) os)))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x '(1 2 3))]
  (assert (= (-unify empty-s x '(1 2 3)) os)))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x {})]
  (assert (= (-unify empty-s x {}) os)))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x {1 2 3 4})]
  (assert (= (-unify empty-s x {1 2 3 4}) os)))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x #{})]
  (assert (= (-unify empty-s x #{}) os)))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x #{1 2 3})]
  (assert (= (-unify empty-s x #{1 2 3}) os)))

;; -----------------------------------------------------------------------------
;; unify with lcons

(let [x (lvar 'x)]
  (assert (= (-unify empty-s (lcons 1 x) 1) false)))

(let [x (lvar 'x)
      y (lvar 'y)
      l (lcons 1 y)
      os (-ext-no-check empty-s x l)]
  (assert (= (-unify empty-s l x) os)))

(let [x (lvar 'x)
      y (lvar 'y)
      lc1 (lcons 1 x)
      lc2 (lcons 1 y)
      os (-ext-no-check empty-s x y)]
  (assert (= (-unify empty-s lc1 lc2) os)))

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
  (assert (= (-unify empty-s lc1 lc2) os)))

(let [x (lvar 'x)
      y (lvar 'y)
      lc1 (lcons 1 (lcons 2 x))
      lc2 (lcons 1 (lcons 2 (lcons 3 y)))
      os (-ext-no-check empty-s x (lcons 3 y))]
  (assert (= (-unify empty-s lc1 lc2) os)))

(let [x (lvar 'x)
      y (lvar 'y)
      lc1 (lcons 1 (lcons 2 x))
      lc2 (lcons 1 (lcons 3 (lcons 4 y)))]
  (assert (= (-unify empty-s lc1 lc2) false)))

(let [x (lvar 'x)
      y (lvar 'y)
      lc2 (lcons 1 (lcons 2 x))
      lc1 (lcons 1 (lcons 3 (lcons 4 y)))]
  (assert (= (-unify empty-s lc1 lc2) false)))

(let [x (lvar 'x)
      y (lvar 'y)
      lc1 (lcons 1 (lcons 2 x))
      lc2 (lcons 1 (lcons 2 y))
      os (-ext-no-check empty-s x y)]
  (assert (= (-unify empty-s lc1 lc2) os)))

(let [x (lvar 'x)
      lc1 (lcons 1 (lcons 2 x))
      l1 '(1 2 3 4)
      os (-ext-no-check empty-s x '(3 4))]
  (assert (= (-unify empty-s lc1 l1) os)))

(let [x (lvar 'x)
      y (lvar 'y)
      lc1 (lcons 1 (lcons y (lcons 3 x)))
      l1 '(1 2 3 4)
      os (-> empty-s
             (-ext-no-check y 2)
             (-ext-no-check x '(4)))]
  (assert (= (-unify empty-s lc1 l1) os)))

(let [x (lvar 'x)
      lc1 (lcons 1 (lcons 2 (lcons 3 x)))
      l1 '(1 2 3)
      os (-ext-no-check empty-s x '())]
  (assert (= (-unify empty-s lc1 l1) os)))

(let [x (lvar 'x)
      lc1 (lcons 1 (lcons 3 x))
      l1 '(1 2 3 4)]
  (assert (= (-unify empty-s lc1 l1) false)))

(let [x (lvar 'x)
      lc1 (lcons 1 (lcons 2 x))
      l1 '(1 3 4 5)]
  (assert (= (-unify empty-s lc1 l1) false)))

(assert (= (-unify empty-s (lcons 1 (lvar 'x)) {}) false))
(assert (= (-unify empty-s (lcons 1 (lvar 'x)) #{}) false))

;; -----------------------------------------------------------------------------
;; unify with sequential

(assert (= (-unify empty-s '() 1) false))
(assert (= (-unify empty-s [] 1) false))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x [])]
  (assert (= (-unify empty-s [] x) os)))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x [])]
  (assert (= (-unify empty-s [] x) os)))

(let [x (lvar 'x)
      lc1 (lcons 1 (lcons 2 x))
      l1 '(1 2 3 4)
      os (-ext-no-check empty-s x '(3 4))]
  (assert (= (-unify empty-s l1 lc1) os)))

(assert (= (-unify empty-s [1 2 3] [1 2 3]) empty-s))
(assert (= (-unify empty-s '(1 2 3) [1 2 3]) empty-s))
(assert (= (-unify empty-s '(1 2 3) '(1 2 3)) empty-s))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x 2)]
  (assert (= (-unify empty-s `(1 ~x 3) `(1 2 3)) os)))

(assert (= (-unify empty-s [1 2] [1 2 3]) false))
(assert (= (-unify empty-s '(1 2) [1 2 3]) false))
(assert (= (-unify empty-s [1 2 3] [3 2 1]) false))
(assert (= (-unify empty-s '() '()) empty-s))
(assert (= (-unify empty-s '() '(1)) false))
(assert (= (-unify empty-s '(1) '()) false))
(assert (= (-unify empty-s [[1 2]] [[1 2]]) empty-s))
(assert (= (-unify empty-s [[1 2]] [[2 1]]) false))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x 1)]
  (assert (= (-unify empty-s [[x 2]] [[1 2]]) os))) ;; false

(let [x (lvar 'x)
      os (-ext-no-check empty-s x [1 2])]
  (assert (= (-unify empty-s [x] [[1 2]]) os)))

(let [x (lvar 'x) y (lvar 'y)
      u (lvar 'u) v (lvar 'v)
      os (-> empty-s
             (-ext-no-check y 'a)
             (-ext-no-check x 'b))]
  (assert (= (-unify empty-s ['a x] [y 'b]) os)))

(assert (= (-unify empty-s [] {}) false))
(assert (= (-unify empty-s '() {}) false))
(assert (= (-unify empty-s [] #{}) false))
(assert (= (-unify empty-s '() #{}) false))

;; -----------------------------------------------------------------------------
;; unify with map

(assert (= (-unify empty-s {} 1) false))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x {})]
  (assert (= (-unify empty-s {} x) os)))

(let [x (lvar 'x)]
  (assert (= (-unify empty-s {} (lcons 1 x)) false)))

(assert (= (-unify empty-s {} '()) false))
(assert (= (-unify empty-s {} {}) empty-s))
(assert (= (-unify empty-s {1 2 3 4} {1 2 3 4}) empty-s))
(assert (= (-unify empty-s {1 2} {1 2 3 4}) false))

(let [x (lvar 'x)
      m1 {1 2 3 4}
      m2 {1 2 3 x}
      os (-ext-no-check empty-s x 4)]
  (assert (= (-unify empty-s m1 m2) os)))

(let [x (lvar 'x)
      m1 {1 2 3 4}
      m2 {1 4 3 x}]
  (assert (= (-unify empty-s m1 m2) false)))

(assert (= (-unify empty-s {} #{}) false))

;; -----------------------------------------------------------------------------
;; unify with set

(assert (= (-unify empty-s #{} 1) false))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x #{})]
  (assert (= (-unify empty-s #{} x) os)))

(let [x (lvar 'x)]
  (assert (= (-unify empty-s #{} (lcons 1 x)) false)))

(assert (= (-unify empty-s #{} '()) false))

(assert (= (-unify empty-s #{} {}) false))

(assert (= (-unify empty-s #{} #{}) empty-s))

(assert (= (-unify empty-s #{} #{1}) false))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x 1)]
  (assert (= (-unify empty-s #{x} #{1}) os)))

(let [x (lvar 'x)
      y (lvar 'y)
      os (-> empty-s
             (-ext-no-check x 2)
             (-ext-no-check y 1))]
  (assert (= (-unify empty-s #{1 x} #{2 y}) os)))

(let [x (lvar 'x)
      y (lvar 'y)
      os (-> empty-s
             (-ext-no-check x 2)
             (-ext-no-check y 1))]
  (assert (= (-unify empty-s #{x 1} #{2 y}) os)))

(let [a (lvar 'a)
      b (lvar 'b)
      c (lvar 'c)
      d (lvar 'd)
      s (.-s (-unify empty-s #{a b 3 4 5} #{1 2 3 c d}))]
  (assert (and (= (count s) 4)
               (= (set (map #(.-lhs %) s)) #{a b c d})
               (= (set (map #(.-rhs %) s)) #{1 2 4 5}))))

(let [a (lvar 'a)
      b (lvar 'b)
      c (lvar 'c)
      d (lvar 'd)]
  (assert (= (-unify empty-s #{a b 9 4 5} #{1 2 3 c d}) false)))

;; =============================================================================
;; walk

(assert (= (let [x (lvar 'x)
                 y (lvar 'y)
                 s (to-s [[x 5] [y x]])]
             (-walk s y))
           5))

(assert (= (let [[x y z c b a :as s] (map lvar '[x y z c b a])
                 s (to-s [[x 5] [y x] [z y] [c z] [b c] [a b]])]
             (-walk s a))
           5))

;; =============================================================================
;; reify

(assert (= (let [x (lvar 'x)
                 y (lvar 'y)]
             (-reify-lvar-name (to-s [[x 5] [y x]])))
           '_.2))

;; =============================================================================
;; walk*

(assert (= (let [x (lvar 'x)
                 y (lvar 'y)]
             (-walk* (to-s [[x 5] [y x]]) `(~x ~y)))
           '(5 5)))

;; =============================================================================
;; run and unify

(assert (= (run* [q]
             (m/== true q))
           '(true)))

(assert (= (run* [q]
             (fresh [x y]
               (m/== [x y] [1 5])
               (m/== [x y] q)))
           [[1 5]]))

(assert (= (run* [q]
             (fresh [x y]
               (m/== [x y] q)))
           '[[_.0 _.1]]))

;; =============================================================================
;; fail

(assert (= (run* [q]
             fail
             (m/== true q))
           []))

;; =============================================================================
;; Basic

(assert (= (run* [q]
             (all
              (m/== 1 1)
              (m/== q true)))
           '(true)))

;; =============================================================================
;; TRS

(defn pairo [p]
  (fresh [a d]
    (m/== (lcons a d) p)))

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
    [(emptyo s) (m/== '() out)]
    [(pairo s)
     (fresh [a d res-a res-d]
       (conso a d s)
       (flatteno a res-a)
       (flatteno d res-d)
       (appendo res-a res-d out))]
    [(conso s '() out)]))

(defn rembero [x l out]
  (conde
    [(m/== '() l) (m/== '() out)]
    [(fresh [a d]
       (conso a d l)
       (m/== x a)
       (m/== d out))]
    [(fresh [a d res]
       (conso a d l)
       (conso a res out)
       (rembero x d res))]))

;; =============================================================================
;; conde

(assert (= (run* [x]
             (conde
               [(m/== x 'olive) succeed]
               [succeed succeed]
               [(m/== x 'oil) succeed]))
           '[olive _.0 oil]))

(assert (= (run* [r]
             (fresh [x y]
               (conde
                 [(m/== 'split x) (m/== 'pea y)]
                 [(m/== 'navy x) (m/== 'bean y)])
               (m/== (cons x (cons y ())) r)))
           '[(split pea) (navy bean)]))

(defn teacupo [x]
  (conde
    [(m/== 'tea x) s#]
    [(m/== 'cup x) s#]))

(assert (= (run* [r]
             (fresh [x y]
               (conde
                 [(teacupo x) (m/== true y) s#]
                 [(m/== false x) (m/== true y)])
               (m/== (cons x (cons y ())) r)))
           '((false true) (tea true) (cup true))))

;; =============================================================================
;; conso

(assert (= (run* [q]
             (fresh [a d]
               (conso a d ())
               (m/== (cons a d) q)))
           []))

(let [a (lvar 'a)
      d (lvar 'd)]
  (assert (= (run* [q]
               (conso a d q))
             [(lcons a d)])))

(assert (= (run* [q]
             (m/== [q] nil))
           []))

(assert (=
         (run* [q]
           (conso 'a nil q))
         '[(a)]))

(assert (= (run* [q]
             (conso 'a '(d) q))
           '[(a d)]))

(assert (= (run* [q]
             (conso 'a q '(a)))
           '[()]))

(assert (= (run* [q]
             (conso q '(b c) '(a b c)))
           '[a]))

;; =============================================================================
;; firsto

(assert (= (run* [q]
             (firsto q '(1 2)))
           (list (lcons '(1 2) (lvar 'x)))))

;; =============================================================================
;; resto

(assert (= (run* [q]
             (resto q '(1 2)))
           '[(_.0 1 2)]))

(assert (= (run* [q]
             (resto q [1 2]))
           '[(_.0 1 2)]))

(assert (= (run* [q]
             (resto [1 2] q))
           '[(2)]))

(assert (= (run* [q]
             (resto [1 2 3 4 5 6 7 8] q))
           '[(2 3 4 5 6 7 8)]))

;; =============================================================================
;; flatteno

(assert (= (run* [x]
             (flatteno '[[a b] c] x))
           '(([[a b] c]) ([a b] (c)) ([a b] c) ([a b] c ())
             (a (b) (c)) (a (b) c) (a (b) c ()) (a b (c))
             (a b () (c)) (a b c) (a b c ()) (a b () c)
             (a b () c ()))))

;; =============================================================================
;; membero

(assert (= (run* [q]
             (all
              (m/== q [(lvar)])
              (membero ['foo (lvar)] q)
              (membero [(lvar) 'bar] q)))
           '([[foo bar]])))

(assert (= (run* [q]
             (all
              (m/== q [(lvar) (lvar)])
              (membero ['foo (lvar)] q)
              (membero [(lvar) 'bar] q)))
           '([[foo bar] _.0] [[foo _.0] [_.1 bar]]
               [[_.0 bar] [foo _.1]] [_.0 [foo bar]])))

;; -----------------------------------------------------------------------------
;; rembero

(assert (= (run 1 [q]
             (rembero 'b '(a b c b d) q))
           '((a c b d))))

;; -----------------------------------------------------------------------------
;; conde clause count

(defn digit-1 [x]
  (conde
    [(m/== 0 x)]))

(defn digit-4 [x]
  (conde
    [(m/== 0 x)]
    [(m/== 1 x)]
    [(m/== 2 x)]
    [(m/== 3 x)]))

(assert (= (run* [q]
             (fresh [x y]
               (digit-1 x)
               (digit-1 y)
               (m/== q [x y])))
           '([0 0])))

(assert (= (run* [q]
             (fresh [x y]
               (digit-4 x)
               (digit-4 y)
               (m/== q [x y])))
           '([0 0] [0 1] [0 2] [1 0] [0 3] [1 1] [1 2] [2 0]
               [1 3] [2 1] [3 0] [2 2] [3 1] [2 3] [3 2] [3 3])))

;; -----------------------------------------------------------------------------
;; anyo

(defn anyo [q]
  (conde
    [q s#]
    [(anyo q)]))

(assert (= (run 1 [q]
             (anyo s#)
             (m/== true q))
           (list true)))

(assert (= (run 5 [q]
             (anyo s#)
             (m/== true q))
           (list true true true true true)))

;; -----------------------------------------------------------------------------
;; divergence

(def f1 (fresh [] f1))

(assert (= (run 1 [q]
             (conde
               [f1]
               [(m/== false false)]))
           '(_.0)))

(assert (= (run 1 [q]
             (conde
               [f1 (m/== false false)]
               [(m/== false false)]))
           '(_.0)))

(def f2
  (fresh []
    (conde
      [f2 (conde
            [f2]
            [(m/== false false)])]
      [(m/== false false)])))

(assert (= (run 5 [q] f2)
           '(_.0 _.0 _.0 _.0 _.0)))

;; -----------------------------------------------------------------------------
;; conda (soft-cut)

(assert (= (run* [x]
             (conda
               [(m/== 'olive x) s#]
               [(m/== 'oil x) s#]
               [u#]))
           '(olive)))

(assert (= (run* [x]
             (conda
               [(m/== 'virgin x) u#]
               [(m/== 'olive x) s#]
               [(m/== 'oil x) s#]
               [u#]))
           '()))

(assert (= (run* [x]
             (fresh (x y)
               (m/== 'split x)
               (m/== 'pea y)
               (conda
                 [(m/== 'split x) (m/== x y)]
                 [s#]))
             (m/== true x))
           '()))

(assert (= (run* [x]
             (fresh (x y)
               (m/== 'split x)
               (m/== 'pea y)
               (conda
                 [(m/== x y) (m/== 'split x)]
                 [s#]))
             (m/== true x))
           '(true)))

(defn not-pastao [x]
  (conda
    [(m/== 'pasta x) u#]
    [s#]))

(assert (= (run* [x]
             (conda
               [(not-pastao x)]
               [(m/== 'spaghetti x)]))
           '(spaghetti)))

;; -----------------------------------------------------------------------------
;; condu (committed-choice)

(comment
  (defn onceo [g]
    (condu
      (g s#)))

  (assert (= (run* [x]
               (onceo (teacupo x)))
             '(tea)))

  (assert (= (run* [r]
               (conde
                 [(teacupo r) s#]
                 [(m/== false r) s#]))
             '(false tea cup)))

  (assert (= (run* [r]
               (conda
                 [(teacupo r) s#]
                 [(m/== false r) s#]))
             '(tea cup)))
  )

;; -----------------------------------------------------------------------------
;; nil in collection

(comment
  (assert (= (run* [q]
               (m/== q [nil]))
             '([nil])))

 (assert (= (run* [q]
              (m/== q [1 nil]))
            '([1 nil])))

 (assert (= (run* [q]
              (m/== q [nil 1]))
            '([nil 1])))

 (assert (= (run* [q]
              (m/== q '(nil)))
            '((nil))))

 (assert (= (run* [q]
              (m/== q {:foo nil}))
            '({:foo nil})))

 (assert (= (run* [q]
              (m/== q {nil :foo}))
            '({nil :foo})))
 )

;; -----------------------------------------------------------------------------
;; Occurs Check

(assert (= (run* [q]
             (m/== q [q]))
           ()))

;; -----------------------------------------------------------------------------
;; Unifications that should fail

(assert (= (run* [p]
             (fresh [a b]
               (m/== b ())
               (m/== '(0 1) (lcons a b))
               (m/== p [a b])))
           ()))

(assert (= (run* [p]
             (fresh [a b]
               (m/== b '(1))
               (m/== '(0) (lcons a b))
               (m/== p [a b])))
           ()))

(assert (= (run* [p]
             (fresh [a b c d]
               (m/== () b)
               (m/== '(1) d)
               (m/== (lcons a b) (lcons c d))
               (m/== p [a b c d])))
           ()))

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
  (all
   (m/== [(lvar) (lvar) [(lvar) (lvar) 'milk (lvar) (lvar)] (lvar) (lvar)] hs)
   (firsto hs ['norwegian (lvar) (lvar) (lvar) (lvar)])
   (nexto ['norwegian (lvar) (lvar) (lvar) (lvar)] [(lvar) (lvar) (lvar) (lvar) 'blue] hs)
   (righto [(lvar) (lvar) (lvar) (lvar) 'ivory] [(lvar) (lvar) (lvar) (lvar) 'green] hs)
   (membero ['englishman (lvar) (lvar) (lvar) 'red] hs)
   (membero [(lvar) 'kools (lvar) (lvar) 'yellow] hs)
   (membero ['spaniard (lvar) (lvar) 'dog (lvar)] hs)
   (membero [(lvar) (lvar) 'coffee (lvar) 'green] hs)
   (membero ['ukrainian (lvar) 'tea (lvar) (lvar)] hs)
   (membero [(lvar) 'lucky-strikes 'oj (lvar) (lvar)] hs)
   (membero ['japanese 'parliaments (lvar) (lvar) (lvar)] hs)
   (membero [(lvar) 'oldgolds (lvar) 'snails (lvar)] hs)
   (nexto [(lvar) (lvar) (lvar) 'horse (lvar)] [(lvar) 'kools (lvar) (lvar) (lvar)] hs)
   (nexto [(lvar) (lvar) (lvar) 'fox (lvar)] [(lvar) 'chesterfields (lvar) (lvar) (lvar)] hs)))

(println (pr-str (run 1 [q] (zebrao q))))
(time (run 1 [q] (zebrao q)))

(println "ok")