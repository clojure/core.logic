(ns clojure.core.logic.nominal.tests
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]
        clojure.test :reload)
  (:require [clojure.pprint :as pp]))

;; =============================================================================
;; nominal unification

(deftest test-nom-1
  (is (= (run* [q] (nom/fresh [a] (== a a))) '(_0)))
  (is (= (run* [q] (nom/fresh [a] (== q a))) '(a_0)))
  (is (= (run* [q] (nom/fresh [a] (== a q))) '(a_0)))
  (is (= (run* [q] (nom/fresh [a b] (== q [a b]))) '([a_0 a_1])))
  (is (= (run* [q] (nom/fresh [a b] (conde [(== q a)] [(== q b)]))) '(a_0 a_0)))
  (is (= (run* [q] (nom/fresh [a b] (== a b))) '()))
  (is (= (run* [q] (nom/fresh [a] (== a 1))) '()))
  (is (= (run* [q] (nom/fresh [a] (== 1 a))) '()))
  (is (= (run* [q] (nom/fresh [a] (== nil a))) '()))
  (is (= (run* [q] (nom/fresh [a] (== a nil))) '()))
  (is (= (run* [q] (nom/fresh [a] (== q 1) (== q a))) '()))
  (is (= (run* [q] (nom/fresh [a] (== q a) (== q 1))) '()))
  (is (= (run* [q] (nom/fresh [a b] (== a q) (== b q))) '()))
  (is (= (run* [q] (nom/fresh [a] (predc a number? `number?))) '()))
  (is (= (run* [q] (nom/fresh [a] (predc q number? `number?) (== q a))) '()))
  (is (= (run* [q] (nom/fresh [a]  (== q a) (predc q number? `number?))) '())))

(deftest test-nom-2
  (is (= (run* [q] (nom/fresh [a b] (nom/hash a q) (nom/hash b q))) '(_0)))
  (is (= (run* [q] (fresh [x]  (nom/fresh [a b] (nom/hash a x) (nom/hash b x) (== [x a b] q))))
        '(([_0 a_1 a_2] :- a_2#_0 a_1#_0))))
  (is (= (run* [q] (fresh [x] (nom/fresh [a] (nom/hash a q) (== q x)))) '(_0)))
  (is (= (run* [q] (fresh [x y] (nom/fresh [a] (nom/hash a x) (== y x) (== [y a] q))))
        '(([_0 a_1] :- a_1#_0))))
  (is (= (run* [q] (fresh [x] (nom/fresh [a] (nom/hash a q) (== q `(~x))))) '((_0))))
  (is (= (run* [q] (fresh [x y] (nom/fresh [a] (nom/hash a y) (== y `(~x)) (== [y a] q))))
        '(([(_0) a_1] :- a_1#_0))))
  (is (= (run* [q] (fresh [x y z] (nom/fresh [a] (nom/hash a q) (== q `(~x ~y))))) '((_0 _1))))
  (is (= (run* [q] (fresh [x y z] (nom/fresh [a] (nom/hash a z) (== z `(~x ~y)) (== [z a] q))))
        '(([(_0 _1) a_2] :- a_2#_1 a_2#_0))))
  (is (= (run* [q] (fresh [x y] (nom/fresh [a] (nom/hash a q) (conso x y q)))) `(~(lcons '_0 '_1))))
  (is (= (run* [q] (fresh [x y z] (nom/fresh [a] (nom/hash a z) (conso x y z) (== [z a] q))))
        [[[(lcons '_0 '_1) 'a_2] ':- 'a_2#_1 'a_2#_0]]))
  (is (= (run* [q] (fresh [x y] (nom/fresh [a]  (conso x y q) (nom/hash a q)))) `(~(lcons '_0 '_1))))
  (is (= (run* [q] (fresh [x y z] (nom/fresh [a] (nom/hash a z) (conso x y z) (== [z a] q))))
        [[[(lcons '_0 '_1) 'a_2] ':- 'a_2#_1 'a_2#_0]]))
  (is (= (run* [q] (nom/fresh [a b] (== q nil) (nom/hash a q))) '(nil)))
  (is (= (run* [q] (nom/fresh [a b] (== q 1) (nom/hash a q))) '(1)))
  (is (= (run* [q] (nom/fresh [a b] (== q [1 1]) (nom/hash a q))) '([1 1])))
  (is (= (run* [q] (nom/fresh [a b] (== q (lcons 1 1)) (nom/hash a q))) [(lcons 1 1)]))
  (is (= (run* [q] (nom/fresh [a b] (nom/hash a q) (== q (lcons 1 1)))) [(lcons 1 1)]))
  (is (= (run* [q] (nom/fresh [a b] (== q b) (nom/hash a q))) '(a_0)))
  (is (= (run* [q] (nom/fresh [a b] (nom/hash a q) (== q b))) '(a_0)))
  (is (= (run* [q] (nom/fresh [a b] (conde [(== q a) (nom/hash b q)] [(== q b)]))) '(a_0 a_0)))
  (is (= (run* [q] (nom/fresh [a] (nom/hash a a)))) '())
  (is (= (run* [q] (nom/fresh [a] (== q a) (nom/hash a q))) '()))
  (is (= (run* [q] (nom/fresh [a] (nom/hash a q) (== q a))) '()))
  (is (= (run* [q] (nom/fresh [a] (nom/hash a `(~a)))) '()))
  (is (= (run* [q] (nom/fresh [a] (== q `(~a)) (nom/hash a q))) '()))
  (is (= (run* [q] (nom/fresh [a] (nom/hash a q) (== q `(~a)))) '())))

(deftest test-nom-3
  (is (= (run* [q] (nom/fresh [a] (nom/hash a (nom/tie a a)))) '(_0)))
  (is (= (run* [q] (nom/fresh [a b] (nom/hash a (nom/tie a b)))) '(_0)))
  (is (= (run* [q] (nom/fresh [a b] (nom/hash a `(~b ~(nom/tie a a))))) '(_0)))
  (is (= (run* [q] (nom/fresh [a] (== q (nom/tie a a)) (nom/hash a q))) [(nom/tie 'a_0 'a_0)]))
  (is (= (run* [q] (nom/fresh [a b] (== q (nom/tie a b)) (nom/hash a q))) [(nom/tie 'a_0 'a_1)]))
  (is (= (run* [q] (nom/fresh [a b] (== q `(~b ~(nom/tie a a))) (nom/hash a q))) [['a_0 (nom/tie 'a_1 'a_1)]]))
  (is (= (run* [q] (nom/fresh [a b] (nom/hash a (nom/tie b a)))) '()))
  (is (= (run* [q] (nom/fresh [a b] (nom/hash a `(~a ~(nom/tie a a))))) '()))
  (is (= (run* [q] (nom/fresh [a b] (nom/hash a `(~b ~(nom/tie b a))))) '()))
  (is (= (run* [q] (nom/fresh [a b] (conde
                                      [(nom/hash a `(~b ~(nom/tie b a)))]
                                      [(== q (nom/tie a b)) (nom/hash a q)]
                                      [(== q `(~b ~(nom/tie a a))) (nom/hash a q)])))
         [(nom/tie 'a_0 'a_1) ['a_0 (nom/tie 'a_1 'a_1)]])))

(deftest test-nom-4
  (is (= (run* [q] (nom/fresh [a] (== (nom/tie a a) (nom/tie a a)))) '(_0)))
  (is (= (run* [q] (nom/fresh [a b] (== (nom/tie a a) (nom/tie b b)))) '(_0)))
  (is (= (run* [q] (nom/fresh [a] (== q (nom/tie a a)))) [(nom/tie 'a_0 'a_0)]))
  (is (= (run* [q] (nom/fresh [a b] (== q (nom/tie a ['foo a 3 b])))) [(nom/tie 'a_0 ['foo 'a_0 3 'a_1])]))
  (is (= (run* [q] (nom/fresh [a b] (== (nom/tie a q) (nom/tie b b)))) '(a_0)))
  (is (= (run* [q]
           (nom/fresh [a b]
             (fresh [x y]
               (== (nom/tie a (nom/tie a x)) (nom/tie a (nom/tie b y)))
               (== [x y] q))))
         [[[(susp '((a_0 a_1)) '_2) '_2] ':- 'a_0#_2]]))
  (is (= (run* [q]
           (nom/fresh [a b]
             (fresh [x y]
               (== (nom/tie a (nom/tie b [y b])) (nom/tie b (nom/tie a [a x])))
               (== [x y] q))))
         '((a_0 a_1))))
  (is (= (run* [q]
           (nom/fresh [a b]
             (fresh [x y]
               (== (nom/tie a (nom/tie b [b y])) (nom/tie b (nom/tie a [a x])))
               (== [x y] q))))
         [['_0 (susp '((a_1 a_2)) '_0)]]))
  (is (= (run* [q]
           (nom/fresh [a b]
             (fresh [x y]
               (conde
                 [(== (nom/tie a (nom/tie b [x b]))
                      (nom/tie b (nom/tie a [a x])))]
                 [(== (nom/tie a (nom/tie b [y b]))
                      (nom/tie b (nom/tie a [a x])))]
                 [(== (nom/tie a (nom/tie b [b y]))
                      (nom/tie b (nom/tie a [a x])))]
                 [(== (nom/tie a (nom/tie b [b y]))
                      (nom/tie a (nom/tie a [a x])))])
               (== [x y] q))))
         ['(a_0 a_1)
          ['_0 (susp '((a_1 a_2)) '_0)]
          [['_0 (susp '((a_1 a_2)) '_0)] ':- 'a_1#_0]]))
  (is (= (run* [q]
           (fresh [bx by]
             (nom/fresh [x y]
               (== (nom/tie x (nom/tie y by)) (nom/tie x (nom/tie x bx)))
               (== by ['foo q]))))
        '(_0)))
  (is (= (run* [q]
           (fresh [bx by]
             (nom/fresh [x y]
               (== (nom/tie x (nom/tie y by)) (nom/tie x (nom/tie x bx)))
               (== ['foo q] by))))
        '(_0)))
  (is (= (run* [q]
           (nom/fresh [a b c d]
             (fresh [x y w z]
               (== (nom/tie a (nom/tie b [b y])) (nom/tie b (nom/tie a [a x])))
               (== (nom/tie c (nom/tie d [d z])) (nom/tie d (nom/tie c [c w])))
               (== y z)
               (== y z)
               )))
        '(_0)))
  (is (= (run* [q]
           (nom/fresh [a b]
             (fresh [x y]
               (== (nom/tie a (nom/tie b [b [y]])) (nom/tie b (nom/tie a [a [x]])))
               (conso 1 y q)
               (== y [1]))))
        '((1 1)))))

(deftest test-nom-5
  (is (= (run* [q]
           (fresh [t u]
             (nom/fresh [a b c d]
               (== t ['lam (nom/tie a ['lam (nom/tie b ['var a])])])
               (== u ['lam (nom/tie c ['lam (nom/tie d ['var c])])])
               (== t u))))
         '(_0)))
  (is (= (run* [q]
           (fresh [t u]
             (nom/fresh [a b c d]
               (== t ['lam (nom/tie a ['lam (nom/tie b ['var a])])])
               (== u ['lam (nom/tie c ['lam (nom/tie d ['var d])])])
               (== t u))))
        '()))
  (is (= (run* [q]
           (fresh [x e1 e2]
             (nom/fresh [a b]
               (== x ['lam (nom/tie a e1)])
               (== e1 ['var a])
               (== x ['lam (nom/tie b e2)])
               (== q x))))
        [['lam (nom/tie 'a_0 ['var 'a_0])]]))
  (is (= (run* [q]
           (fresh [x e1 e2]
             (nom/fresh [a b]
               (== x ['lam (nom/tie a e1)])
               (== e1 ['var a])
               (== ['lam (nom/tie b e2)] x)
               (== q x))))
        [['lam (nom/tie 'a_0 ['var 'a_0])]])))

(defn- substo [e new a out]
  (conde
    [(== ['var a] e) (== new out)]
    [(fresh [y]
       (== ['var y] e)
       (== ['var y] out)
       (nom/hash a y))]
    [(fresh [rator ratorres rand randres]
       (== ['app rator rand] e)
       (== ['app ratorres randres] out)
       (substo rator new a ratorres)
       (substo rand new a randres))]
    [(fresh [body bodyres]
       (nom/fresh [c]
         (== ['lam (nom/tie c body)] e)
         (== ['lam (nom/tie c bodyres)] out)
         (nom/hash c a)
         (nom/hash c new)
         (substo body new a bodyres)))]))

(deftest test-nom-6
  (is (= (run* [q]
           (nom/fresh [a b]
             (substo ['lam (nom/tie a ['app ['var a] ['var b]])]
               ['var b]
               a
               q)))
         [['lam (nom/tie 'a_0 '(app (var a_0) (var a_1)))]]))
  (is (= (run* [q]
           (nom/fresh [a b]
             (substo ['lam (nom/tie a ['var b])]
               ['var a]
               b
               q)))
         [['lam (nom/tie 'a_0 '(var a_1))]])))

(defn- lookupo [x tx g]
  (fresh [a d]
    (conso a d g)
    (conde
      [(== [x tx] a)]
      [(fresh [xc txc]
         (== [xc txc] a)
         (nom/hash x xc)
         (lookupo x tx d))])))

(defn- typo [g e te]
  (conde
    [(fresh [x]
       (== ['var x] e)
       (lookupo x te g))]
    [(fresh [rator trator rand trand]
       (== ['app rator rand] e)
       (== ['-> trand te] trator)
       (typo g rator trator)
       (typo g rand trand))]
    [(fresh [ec tec trand gc]
       (nom/fresh [b]
         (== ['lam (nom/tie b ec)] e)
         (== ['-> trand tec] te)
         (nom/hash b g)
         (conso [b trand] g gc)
         (typo gc ec tec)))]))

(deftest test-nom-7
  (is (= (run* [q]
           (nom/fresh [c d]
             (typo [] ['lam (nom/tie c ['lam (nom/tie d ['var c])])] q)))
         '((-> _0 (-> _1 _0)))))
  (is (= (run* [q]
           (nom/fresh [c]
             (typo [] ['lam (nom/tie c ['app ['var c] ['var c]])] q))))
      '())
  (is (= (run 2 [q] (typo [] q '(-> int int)))
         [['lam (nom/tie 'a_0 '(var a_0))]
          ['lam (nom/tie 'a_0 ['app ['lam (nom/tie 'a_1 '(var a_1))] '(var a_0)])]])))

(deftest test-nom-mix-1
  (is (= (run* [q] (nom/fresh [a b] (!= a b))) '(_0)))
  (is (= (run* [q]
           (nom/fresh [a b]
             (fresh [x y]
               (== (nom/tie a (nom/tie b [b y])) (nom/tie b (nom/tie a [a x])))
               (infd x (interval 1 3))
               (== [x y] q))))
        '([1 1] [2 2] [3 3])))
  (is (= (run* [q]
           (nom/fresh [a b]
             (fresh [x y]
               (== (nom/tie a (nom/tie b [b y])) (nom/tie b (nom/tie a [a x])))
               (infd y (interval 1 3))
               (== [x y] q))))
        '([1 1] [2 2] [3 3])))
  (is (= (run* [q]
           (nom/fresh [a b]
             (fresh [x y]
               (== (nom/tie a (nom/tie b [b y])) (nom/tie b (nom/tie a [a x])))
               (== x a)
               (!= x y)
               (== [x y] q))))
        '([a_0 a_1])))
  (is (= (run* [q]
           (nom/fresh [a b]
             (fresh [x y]
               (== (nom/tie a (nom/tie b [b y])) (nom/tie b (nom/tie a [a x])))
               ;; TODO(namin): unfortunately, reversing the order of constraints changes semantics.
               ;;   commenting out next constraint to get intended result.
               ;; (!= x y)
               (== x a)
               (== [x y] q))))
        '([a_0 a_1])))
  (is (= (run* [q]
           (nom/fresh [a b]
             (fresh [x y]
               (== (nom/tie a (nom/tie b [b y])) (nom/tie b (nom/tie a [a x])))
               (== y 'foo)
               (predc x number? `number?)
               (== [x y] q))))
        '()))
  (is (= (run* [q]
           (nom/fresh [a b]
             (fresh [x y]
               (== (nom/tie a (nom/tie b [b y])) (nom/tie b (nom/tie a [a x])))
               (predc x number? `number?)
               (== y 'foo)
               (== [x y] q))))
        '())))

(deftest test-nom-impl-1
  (let [a (nom (lvar 'a))
        b (nom (lvar 'b))
        c (nom (lvar 'c))
        d (nom (lvar 'd))]
    (is (= (disagreement-set [] []) nil))
    (is (= (disagreement-set [[a b]] [[c d]]) [a b c d]))
    (is (= (disagreement-set [[a b]] [[a b] [c d]]) [c d]))))

