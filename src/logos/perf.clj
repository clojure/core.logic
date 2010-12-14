(ns logos.perf
  (:refer-clojure :exclude [reify inc == take])
  (:use logos.minikanren
        logos.logic
        logos.unify))

(defn teacup-o [x]
  (cond-e
   ((== 'tea x) s*)
   ((== 'cup x) s*)))

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

  ;; 3 million unifications in about 1.2s, not bad
  ;; lazy sequences are significantly slower
  ;; about 3X at 3s
  (dotimes [_ 10]
    (time
     (dotimes [_ 3e6]
       (run* [q]
             (== true q)))))

  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (run* [q]
             (== true q)))))

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
  
  ;; ~560ms!!!
  ;; Scheme at ~1.3s
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (run* [q]
             succeed
             (== true q)))))

  ;; 1s for 1e5
  ;; 2s for Scheme, so Clojure miniKanren is about twice as fast
  (dotimes [_ 10]
   (time
    (dotimes [_ 1e5]
     (run* [r]
           (exist [x y]
                  (cond-e
                   ((teacup-o x) (== true y) s*)
                   ((== false x) (== true y)))
                  (== (cons x (cons y ())) r))))))

  ;; ~200,000 unifications / sec
  ;; what kind of boost could we get with tabling ?
  (dotimes [_ 10]
    (time
     (dotimes [_ 2e5]
       (run* [q]
             (exist [x y]
                    (== [x y] [1 5])
                    (== [x y] q))))))

  ;; 2.5s, much slower
  (dotimes [_ 10]
    (time
     (dotimes [_ 2e5]
       (run* [q]
             (exist [x y z]
                    (== y z)
                    (== [1 2 {x z}] q)
                    (== z 5))))))

  ;; 1.8s, if we had something eliminated the needless unifications
  ;; not that much of an improvement for 2e5
  (dotimes [_ 10]
    (time
     (dotimes [_ 2e5]
       (run* [q]
             (exist [x y z]
                    (== [1 2 {x 5}] q))))))

  ;; this is going to be very, very slow
  ;; postwalk is not cheap
  ;; 45 seconds
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (unifier '{?x ?y} {1 2}))))

  ;; much faster
  (dotimes [_ 10]
    (let [[u w] (map prep ['{?x ?y} {1 2}])]
     (time
      (dotimes [_ 1e5]
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
       (run* [q]
             (append-o '(1 2) '(3 4) q)))))

  ;; Racket clocks ~720ms
  ;; We're seeing ~900ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (run 5 [x]
            (exist [y]
                   (append-o (llist 'cake y) '(d t) x))))))

  ;; ~300ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (run* [q]
             (rest-o [1 2] q)))))

  ;; 3.8s Scheme is 5.5s
  ;; wow removing vectors make hardly any difference
  ;; should consider using vectors
  ;; or even lists
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (run* [x]
             (flatten-o '[[a b] c] x)))))

  ;; 3.2-3.3 if we don't reify result
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (run* [x]
             (exist [y]
              (flatten-o '[[a b] c] y))))))

  (run 5 [x]
       (flatten-o x '[a b c]))

  (dotimes [_ 10]
    (let [x {1 2 3 4 5 6 7 8 9 0}]
     (time
      (dotimes [_ 1e8]
        (count x)))))

  (dotimes [_ 10]
    (let [x [1 2 3 4 5 6 7 8 9 0]]
     (time
      (dotimes [_ 1e8]
        (count x)))))
  )
