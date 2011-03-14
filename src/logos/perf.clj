(ns logos.perf
  (:refer-clojure :exclude [reify inc ==])
  (:require [clojure.set :as set])
  (:use logos.minikanren
        logos.logic
        logos.unify))

(defn teacup-o [x]
  (cond-e
   ((== 'tea x) s#)
   ((== 'cup x) s#)))

(comment
  ;; ==================================================
  ;; PERFORMANCE
  
  ;; sick 183ms on 1.3.0 alph3
  (dotimes [_ 10]
    (let [[x y z :as s] (map lvar '[x y z])
          ss (to-s [[x 5] [y x]])]
      (time
       (dotimes [_ 1e6]
         (ext-no-check ss z y)))))

  ;; ~239ms
  (dotimes [_ 10]
    (let [[x y z :as s] (map lvar '[x y z])
          ss (to-s [[x 5] [y x]])]
      (time
       (dotimes [_ 1e6]
         (ext ss x z)))))

  ;; ~700ms
  ;; just a tiny bit slower than the Scheme version
  (dotimes [_ 10]
    (let [[x y z c b a :as s] (map lvar '[x y z c b a])
          ss (to-s [[x 5] [y x] [z y] [c z] [b c] [a b]])]
      (time
       (dotimes [_ 1e6]
         (walk ss a)))))

  (let [[x y z c b a :as s] (map lvar '[x y z c b a])
          ss (to-s [[x 5] [y x] [z y] [c z] [b c] [a b]])]
    (walk ss a))

  ;; 200ms (NOTE: this jump is because array-map is slower than hash-maps)
  ;; Scheme is ~1650ms
  ;; with defrecord this becomes - MUCH MUCH slower
  (dotimes [_ 10]
    (let [[x m y n z o c p b q a] (map lvar '[x m y n z o c p b q a])
          ss (to-s [[x 5] [m 0] [m 0] [m 0] [m 0]
                    [y x] [n 0] [n 0] [n 0] [n 0]
                    [z y] [o 0] [o 0] [o 0] [o 0]
                    [c z] [p 0] [p 0] [p 0] [p 0]
                    [b c] [q 0] [q 0] [q 0] [q 0]
                    [a b]])]
      (time
       (dotimes [_ 1e6]
         (walk ss a)))))

  ;; < 1.1s
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (doall
        (run* [q]
              (== true q))))))

  ;; 250ms, a lot of overhead for the above
  (dotimes [_ 10]
    (let [q (lvar 'q)]
     (time
      (dotimes [_ 1e6]
        (unify empty-s true q)))))

  ;; hardly any effect in this case
  (binding [*occurs-check* false]
   (dotimes [_ 10]
     (time
      (dotimes [_ 1e6]
        (doall
         (run* [q]
               (== true q)))))))
  
  ;; ~2s
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (doall
        (run* [q]
              (cond-e
               ((== q 1))
               ((== q 2))))))))

  ;; easy optimization unifying with a constant
  ;; convert to regular let
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (doall
        (run* [q]
              (exist [x y]
                     (== x 4)
                     (== y 5)
                     (== true q)))))))
  
  ; 1.1s
  ;; Scheme at ~1.3s
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (doall
        (run* [q]
              succeed
              (== true q))))))

  ;; 2s
  ;; 2s for Scheme
  (dotimes [_ 10]
   (time
    (dotimes [_ 1e5]
      (doall
       (run* [r]
             (exist [x y]
                    (cond-e
                     ((teacup-o x) (== true y) s#)
                     ((== false x) (== true y)))
                    (== (cons x (cons y ())) r)))))))

  ;; ~1.6-2s
  (dotimes [_ 10]
    (time
     (dotimes [_ 2e5]
       (doall
        (run* [q]
              (exist [x y]
                     (== [x y] [1 5])
                     (== [x y] q)))))))

  ;; 4s, much slower
  (dotimes [_ 10]
    (time
     (dotimes [_ 2e5]
       (doall
        (run* [q]
              (exist [x y z]
                     (== y z)
                     (== [1 2 {x z}] q)
                     (== z 5)))))))

  ;; 1.8s, if we had something eliminated the needless unifications
  ;; not that much of an improvement for 2e5
  (dotimes [_ 10]
    (time
     (dotimes [_ 2e5]
       (run* [q]
             (exist [x y z]
                    (== [1 2 {x 5}] q))))))

  ;; 2 orders of magnitude slower than below
  ;; 370ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (unifier' '{x ?y} '{x 1}))))

  ;; 450ms
  (dotimes [_ 10]
    (let [[u w] (map prep ['{?x ?y} {1 2}])]
     (time
      (dotimes [_ 1e6]
        (unifier u w)))))

  (defn rest-o [l d]
    (exist [a]
           (== (cons a d) l)))

  ;; miniKanren under Racket beats us here
  ;; need to look into this
  ;; ~1.4s vs ~1.6s
  ;; under goals branch
  ;; we're competitive with Racket
  ;; little less than 1.4s
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (doall
        (run* [q]
              (append-o '(1 2) '(3 4) q))))))

  ;; 1.2-3s
  (binding [*occurs-check* false]
   (dotimes [_ 10]
     (time
      (dotimes [_ 1e5]
        (doall
         (run* [q]
               (append-o '(1 2) '(3 4) q)))))))

  ;; Racket clocks ~720ms
  ;; We're at 700ms now
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (doall
        (run 5 [x]
             (exist [y]
                    (append-o (llist 'cake y) '(d t) x)))))))

  ;; 630-50ms
  (binding [*occurs-check* false]
   (dotimes [_ 10]
     (time
      (dotimes [_ 1e4]
        (doall
         (run 5 [x]
              (exist [y]
                     (append-o (llist 'cake y) '(d t) x))))))))

  (dotimes [_ 10]
     (time
      (dotimes [_ 1e4]
        (doall
         (run-nc 5 [x]
                 (exist [y]
                        (append-o (llist 'cake y) '(d t) x)))))))

  ;; ~300ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (doall
        (run* [q]
              (rest-o [1 2] q))))))

  ;; ~280ms
  (binding [*occurs-check* false]
   (dotimes [_ 10]
     (time
      (dotimes [_ 1e5]
        (doall
         (run* [q]
               (rest-o [1 2] q)))))))

  ;; 3.3s Racket is 5.5s
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (doall
        (run* [x]
              (flatten-o '[[a b] c] x))))))

  ;; down to < 3.1-2
  (binding [*occurs-check* false]
   (dotimes [_ 10]
     (time
      (dotimes [_ 1e4]
        (doall
         (run* [x]
               (flatten-o '[[a b] c] x)))))))

  (binding [*occurs-check* false]
   (dotimes [_ 10]
     (time
      (dotimes [_ 1e4]
        (doall
         (run* [x]
               (flatten-o '((a b) c) x)))))))

  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (doall
        (run-nc* [x]
                 (flatten-o '[[a b] c] x))))))

  ;; ~3s if we don't reify result
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (doall
        (run* [x]
              (exist [y]
                     (flatten-o '[[a b] c] y)))))))

  ;; down to < 2.6s if we don't reify
  ;; reification is significant chunk of time
  ;; 1s, or 1/6th of all the time
  (binding [*occurs-check* false]
   (dotimes [_ 10]
     (time
      (dotimes [_ 1e4]
        (doall
         (run* [x]
               (exist [y]
                      (flatten-o '[[a b] c] y))))))))

  (dotimes [_ 10]
     (time
      (dotimes [_ 1e4]
        (doall
         (run-nc* [x]
                  (exist [y]
                         (flatten-o '[[a b] c] y)))))))

  ;; ~500ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e8]
       (unify empty-s 1 1))))

  ;; ~250ms
  (dotimes [_ 10]
    (let [x (lvar 'x)]
     (time
      (dotimes [_ 1e6]
        (unify empty-s x true)))))

  ;; 400ms
  (dotimes [_ 10]
    (let [x (lvar 'x)
          v1 [1 2 3]
          v2 [1 2 3]]
     (time
      (dotimes [_ 1e6]
        (unify empty-s v1 v2)))))

  ;; ~450ms
  (dotimes [_ 10]
    (let [x (lvar 'x)
          v1 [1 2 3]
          v2 [1 2 3]]
     (time
      (dotimes [_ 1e7]
        (unify empty-s v1 false)))))

  ;; with new unification the following are nearly twice as
  ;; fast as under 0.2

  ;; 750ms
  (dotimes [_ 10]
    (let [x (lvar 'x)
          v1 `[1 ~x 3]
          v2 `[1 2 3]]
     (time
      (dotimes [_ 1e6]
        (unify empty-s v1 v2)))))

  ;; 770ms
  (dotimes [_ 10]
    (let [x (lvar 'x)
          l1 `(1 ~x 3)
          l2 `(1 2 3)]
     (time
      (dotimes [_ 1e6]
        (unify empty-s l1 l2)))))

  ;; 950ms
  (let [a (lvar 'a)
      b (lvar 'b)
      c (lvar 'c)
      d (lvar 'd)
      s1 #{a b 3 4 5}
      s2 #{1 2 3 c d}]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e5]
         (.s (unify empty-s s1 s2))))))

  ;; 300ms
  ;; this is ten times slower than vectors
  (let [s1 #{}
        s2 #{}]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e5]
         (.s (unify empty-s s1 s2))))))

  ;; 500ms
  (dotimes [_ 10]
    (let [m1 {1 2 3 4}
          m2 {1 2 3 4}]
     (time
      (dotimes [_ 1e6]
        (unify empty-s m1 m2)))))

  ;; -----------------------------------------------------------------------------
  ;; notes on sets and maps
  
  ;; 100ms
  (dotimes [_ 10]
    (let [s #{1 2 3}]
     (time
      (dotimes [_ 1e6]
        (disj s 3)))))

  ;; 600ms
  (dotimes [_ 10]
    (let [s1 #{1 2 3}
          s2 #{3 4 5}]
     (time
      (dotimes [_ 1e6]
        (set/difference s1 s2)))))

  ;; 700ms
  (dotimes [_ 10]
    (let [s1 #{1 2 3}
          s2 #{3 4 5}]
     (time
      (dotimes [_ 1e6]
        (set/intersection s1 s2)))))

  ;; 200ms
  (dotimes [_ 10]
    (let [m {1 2 3 4 5 6 7 8 9 0}]
     (time
      (dotimes [_ 1e7]
        (keys m)))))

  ;; 1.1s
  (dotimes [_ 10]
    (let [m {1 2 3 4 5 6 7 8 9 0}]
     (time
      (dotimes [_ 1e7]
        (assoc m 10 11)))))
  )
