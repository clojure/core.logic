(ns logos.scratch
  (:refer-clojure :exclude [reify ==])
  (:use logos.minikanren))

(def parent
     #{['pam 'bob]
       ['tom 'bob]
       ['tom 'liz]
       ['bob 'ann]
       ['bob 'pat]
       ['pat 'jim]})

(defn parent-o [x y]
  (fn [a]
    (remove nil?
            (map (fn [p]
                   ((== p [x y]) a))
                 parent))))

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

(comment
  ;; we know y is ground
  ;; we can just ext-no-check the bindings for y
  (parent-o 'bob y)
  (== ['bob y] q)
  )

;; 400ms
;; the above is 200 times slower
;; we could get a 100 fold increase in perf
;; for searches we know about

;; we basically want to eliminate unification as much as possible

(dotimes [_ 10]
  (time
   (dotimes [_ 1e7]
     (filter (fn [[x y]] (= x 'bob)) parent))))

(comment
  
  )