(ns logos.scratch
  (:use logos.minikanren))

(declare b)

(defn a [x]
  (if (> x 18000)
    x
    (+ (b (+ x 1)) 1)))

(defn b [x]
  (if (> x 18000)
    x
    (+ (a (+ x 1)) 1)))

(dotimes [_ 10]
  (time
   (dotimes [_ 1000]
     (a 1))))

(def as (repeat (fn ^long [x] (+ x 1))))

(def bs (repeat (fn ^long [x] (+ x 1))))

(def abs (interleave as bs))

;; the below is twice as slow
;; very similar to trampoline
;; POINT IS
;; stack depth does seem to affect performance
(dotimes [_ 10]
  (time
   (dotimes [_ 1000]
     (loop [x 0 abs abs]
       (if (> x 18000)
         x
         (recur ((first abs) x) (next abs)))))))

;; but can handle much larger depths
;; I wonder how much difference this makes
;; when the stack sizes are really deep
(dotimes [_ 10]
  (time
   (dotimes [_ 1000]
     (loop [x 0 abs abs]
       (if (> x 36000)
         x
         (recur (+ ((first abs) x) 1) (next abs)))))))

;; hmm back to the drawing board

(comment
  (def x (cons 'a (lazy-seq
                   (do
                     (println "b")
                     'b))))

  ;; but what about exhausted streams?

  (interleave (interleave '[a b c] '[1 2 3] '[x y z]) '[i j k])

  ;; this allows us to abandaon values
  ;; what we're getting here is removing the need to unwind the stack
  (defn [a b]
    (cons (first a)
          (lazy-seq
           (cons (b) (rest a)))))

  (take lazy-goals
        )
  )