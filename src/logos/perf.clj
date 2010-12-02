(ns logos.perf
  (:refer-clojure :exclude [reify inc == take])
  (:use logos.minikanren))

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
         (lookup ss a)))))

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
         (lookup ss a)))))

  ;; 1.5 million unifications a second, not bad
  (dotimes [_ 10]
    (time
     (dotimes [_ 1.5e6]
       (run* [q]
             (== true q)))))
  
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
                   ((teacup-o x) (== true y) s#)
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

  ;; 2 seconds
  ;; we should add the unassociated check
  (dotimes [_ 10]
    (let [[u w] (map prep ['(?x ?y ?z ?&r) '(1 2 3 4 5 6 7 8 9 0)])] 
      (time
       (dotimes [_ 1e5]
         (unifier u w)))))

  ;; not bad 600ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (run* [q]
             (exist [r]
                    (== `(1 ~r) '(1 2 3 4 5))
                    (== &r q))))))

  (defn rest-o [l d]
    (exist [a]
           (== (cons a d) l)))
  )