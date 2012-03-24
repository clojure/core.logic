(ns cljs.core.logic.tests
  (:use-macros
   [clj.core.logic.macros
    :only [run run* == conde fresh defne matche]])
  (:use
   [cljs.core.logic
    :only [pair lvar lcons -unify -ext-no-check empty-s]]))

(set! *print-fn* js/print)

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
