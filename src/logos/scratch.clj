(ns logos.scratch
  (:refer-clojure :exclude [reify ==])
  (:require [clojure.set :as set])
  (:use logos.minikanren))

;; lazy-seq return immediately, that's just how they work. You can 'put' them in a thread
;; because they return immediately.

;; take is also lazy, you need to force a doall even w/ take

(comment
  (do
    (println "id:" (.getId (Thread/currentThread)))
    (future
     (println "id:" (.getId (Thread/currentThread)))))

  (let [t (future (repeatedly #(.getId (Thread/currentThread))))]
    (println "id:" (.getId (Thread/currentThread)))
    (println (take 1 @t)))

  ;; this works as expected
  (let [t (future (repeatedly #(.getId (Thread/currentThread))))]
    (println "id:" (.getId (Thread/currentThread)))
    (future (println (take 1 @t))))
  )

(defn apply-cont [k v]
  (k v))

(defn snoc-cps [ls x k]
  (apply-cont k (concat ls (list x))))

(defn reverse*-cps [x k]
  (cond
   (nil? (seq x)) (apply-cont k '())
   (seq? (first x)) (reverse*-cps (rest x)
                      (fn [new-tail]
                        (reverse*-cps (first x)
                          (fn [new-head]
                            (snoc-cps new-tail new-head k)))))
   :else (reverse*-cps (rest x)
           (fn [new-tail]
             (snoc-cps new-tail (first x) k)))))

(comment
  (defn reverse*-cps [x k]
    (cond
     (nil? (seq x)) (trampoline #(apply-cont k '[]))
     (seq? (first x)) (recur (rest x)
                        (fn [new-tail]
                          #(reverse*-cps (first x) ; <-- HERE
                            (fn [new-head]
                              (snoc-cps new-tail new-head k)))))
     :else (recur (rest x)
             (fn [new-tail]
               (snoc-cps new-tail (first x) k)))))
  )

;; using trampoline

(defn apply-cont [k v]
  (k v))

(defn snoc-cps [v x k]
  #(apply-cont k (conj v x)))

(defn reverse*-cps [x k]
  (cond
   (nil? (seq x)) (trampoline #(apply-cont k []))
   (seq? (first x)) (recur (rest x)
                      (fn [new-tail]
                        #(reverse*-cps (first x)
                           (fn [new-head]
                             (snoc-cps new-tail new-head k)))))
   :else (recur (rest x)
           (fn [new-tail]
             (snoc-cps new-tail (first x) k)))))

(comment
  ;; works
  (reverse*-cps '((a b) c) identity)

  ;; stack overflow, of course
  (def x (reverse*-cps (range 5e4) identity))

  ;; 100ms
  ;; 3 orders of magnitude slower
  ;; however Racket isn't any faster here
  (dotimes [_ 10]
    (time
     (dotimes [_ 1]
       (reverse*-cps (range 1e3) identity))))

  ;; if we wisely use vectors, we get an order of magnitude better perf
  ;; 700ms so about 7X slower
  (dotimes [_ 10]
    (let [r (range 1e6)]
     (time
      (dotimes [_ 1]
        (reverse*-cps r identity)))))

  (def y (doall (reverse (range 5e4))))

  ;; ~100ms
  (dotimes [_ 10]
    (let [r (range 1e6)]
     (time
      (dotimes [_ 1]
        (doall (reverse r))))))

  ;; even with trampoline we have stack overflow
  ;; because of the initial decent
  )

;; -----------------------------------------------------------------------------
;; Comparing to minikanren ops to map/filter

(comment
  ;; of course faster if we index the child
  (def parent
     #{['pam 'bob]
       ['tom 'bob]
       ['tom 'liz]
       ['bob 'ann]
       ['bob 'pat]
       ['pat 'jim]})

 (def parent
      #{{:name 'pam :parent 'bob :age 9}
        {:name 'tom :parent 'bob :age 11}})

 (def indexed (set/index parent [:parent]))
 (def indexed (set/index parent [:child]))
 (def indexed (set/index parent [:parent :age]))

 ;; interesting this works
 ;; for a logic system we'd want to go both directions
 (def indexed (set/index parent [1]))

 ;; this is less scalable for functors with more than
 ;; two argument, indexing all combinations

 ;; but this is essentially tabling, no ?
 ;; each part is an index
 (defn parent-o [x y]
   (fn [a]
     (remove nil?
             (map (fn [p]
                    ((== p [x y]) a))
                  parent))))

 (defn parent-o [x y]
   (fn [a]
     (remove nil?
             (map (fn [p]
                    ((== p [x y]) a))
                  parent))))

 )

(comment
  (run* [q]
        (exist [x y]
               (parent-o 'bob y)
               (== [x y] q)))

  ;; 1.5s for 100000 runs if db has 6 things in it
  ;; 300ms if only one entry
  ;; so linear with the size of the set
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (run* [q]
             (exist [x y]
                    (parent-o x y)
                    (== [x y] q))))))

  ;; we could make the above much more efficient
  ;; relation of the above nature can know to
  ;; use map/filter

  ;; 900ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (run* [q]
             (exist [x y]
                    (== x 'bob)
                    (parent-o x y)
                    (== [x y] q))))))

  ;; we know y is ground
  ;; we can just ext-no-check the bindings for y
  (parent-o 'bob y)
  (== ['bob y] q)
  )

;; 400ms
;; we basically want to eliminate unification as much as possible

(comment
  ;; 1.5s, much more what I expected
  ;; we could gain an order of magnitude if we could
  ;; compile to map/filter operations
  ;; even faster if we automatically produce indexes
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (doall (filter (fn [[x y]] (= x 'bob)) parent)))))
 )

(comment
  ;; smart parent-o
  (defn parent-o [x y]
    (fn [a]
      (map (fn [[ox oy]] (ext-no-check a y oy))
           (filter (fn [[ox oy]] (= x ox)) parent))))

  (run* [q]
        (parent-o 'bob q))

  ;; 380ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (doall
        (run* [q]
          (parent-o 'bob q))))))

  ;; 250ms, no reification
  ;; 2.3s, not bad
  ;; with tabling probably much more dramatic
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (doall ((parent-o 'bob (lvar 'x)) empty-s)))))
  )