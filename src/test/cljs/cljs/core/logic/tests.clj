(ns cljs.core.logic.tests
  (:use-macros
   [clj.core.logic.macros
    :only [run run* == conde fresh defne matche]])
  (:use
   [cljs.core.logic
    :only [lvar lcons -unify -ext-no-check empty-s]]))

(assert (= (-unify empty-s nil 1) false))

(let [x (lvar 'x)
      os (-ext-no-check empty-s x nil)]
  (assert (= (-unify empty-s nil x) os)))

(let [x (lvar 'x)]
  (assert (= (-unify empty-s nil (lcons 1 x)) false)))

(let [x (lvar 'x)]
  (assert (= (-unify empty-s nil {}) false)))

(let [x (lvar 'x)]
  (assert (= (-unify empty-s nil #{}) false)))
