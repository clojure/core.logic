(ns logos.scratch)

;; I think we can do this, done properly I don't think we'll see much of a per hit
;; the streams are "frozen", it's not like "new" streams are getting introduced.

;; a cond-e with two clauses will produce two streams of goals
;; if the second clause calls a function with a cond-e with
;; two clause the streams will look something like this

;; a0 b0 a1 b'0 a2 b1 a3 b'1

(declare b)

(defn a [x]
  (let [x (inc x)]
   (lazy-seq
    (cons x (b x)))))

(defn b [x]
  (let [x (inc x)]
   (lazy-seq
    (cons x (a x)))))

;; ~700ms
(dotimes [_ 10]
  (let [as (a 1)]
   (time
    (dotimes [_ 10000]
      (loop [f (first as) n (next as)]
        (if (> f 1000)
          f
          (recur (first n) (next n))))))))

; ~350ms, caching of values?
(dotimes [_ 10]
  (let [as (a 1)]
   (time
    (dotimes [_ 10000]
      (nth as 999)))))

;; not much slower
(dotimes [_ 10]
  (time
   (dotimes [_ 10000]
     (let [as (a 1)]
       (nth as 999)))))

(defn a [x]
  (if (> x 1000)
    x
    (b (+ x 1))))

(defn b [x]
  (if (> x 1000)
    x
    (a (+ x 1))))

;; holy crap, hardly faster
(dotimes [_ 10]
  (time
   (dotimes [_ 10000]
     (a 1))))

(comment
  ;; NOTE: tree-seq that is interleaving ?

  (def as ((fn a [x]
             (lazy-seq
              (cons x (a (+ x 1)))))
           1))

  (def bs ((fn b [x]
             (lazy-seq
              (cons x (b (+ x 2)))))
           1))

  (loop [f (first as) n (next as)]
    (if (> f 1000)
      f
      (recur (first n) (next n))))

  (dotimes [_ 10]
    (time
     (dotimes [_ 10]
       (loop [f (first as) n (next as)]
         (if (> f 1000)
           f
           (recur (first n) (next n)))))))

  (declare b)

  (defn a [x]
    (if (> x 1000)
      x
      (b (+ x 1))))

  (defn b [x]
    (if (> x 1000)
      x
      (a (+ x 2))))

  ;; ~240ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 10000]
       (a 1))))

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

  (def lazy-goals (cons choice (lazy-seq (fn []) (lazy-seq (fn [])))))


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

  )