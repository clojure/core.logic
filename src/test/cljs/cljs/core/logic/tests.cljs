(ns cljs.core.logic.tests
  (:use-macros
   [clj.core.logic.macros
    :only [run run* == conde fresh defne matche]])
  (:use
   [cljs.core.logic
    :only [pair lvar lcons -unify -ext-no-check -walk empty-s to-s]]))

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

(assert
 (= (let [x (lvar 'x)
          y (lvar 'y)
          s (to-s [[x 5] [y x]])]
      (-walk s y))
    5))

(assert
 (= (let [[x y z c b a :as s] (map lvar '[x y z c b a])
          s (to-s [[x 5] [y x] [z y] [c z] [b c] [a b]])]
      (-walk s a))
    5))

(println "ok")