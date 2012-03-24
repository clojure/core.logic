(ns cljs.core.logic.tests
  (:use-macros
   [clj.core.logic.macros
    :only [run run* == conde fresh defne matche]])
  (:use
   [cljs.core.logic
    :only [pair lvar lcons -unify -ext-no-check empty-s]]))

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