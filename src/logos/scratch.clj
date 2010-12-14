(ns logos.scratch)

(defn weave
  "Like interleave but continues going even after some sequences are exhausted."
  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1)
            s2 (seq c2)]
        (cond
         (and s1 s2) (cons (first s1)
                           (cons (first s2) 
                                 (weave (rest s1) (rest s2))))
         s1 s1
         s2 s2))))
  ([c1 c2 & colls] 
     (lazy-seq 
      (let [ss (remove nil? (map seq (conj colls c2 c1)))]
        (concat (map first ss) (apply weave (map rest ss)))))))

;; bind maps goal onto a stream of substitutions
;; a choice is simply a substitution in which to
;; look up a lvar

;; what if a bind fails ?
;; hmm got a lot to think about tomorrow, fun stuff
;; time to really understand bind and mplus
;; which cases do we *not* care about ?
;; does bind really to deal with mplus?
;;
;; 1. short circuit on nil
;; 2. 
;; 3. 
;; 4. if interleaved stream, interleave goal with substs and g-rest
;;
;; but we have a real stream, do we need to call a function?
;; or do we just add it to the stream?

(defn bind
  ([ss] ss)
  ([ss g0 & g-rest]
     (if-let [ss' (seq (remove nil? (map g0 ss)))]
       (recur (map g0 ss') g-rest))))

(defmacro cond-e [& clauses]
  (let [a (gensym "a")]
   `(fn [~a]
      (weave ~@(bind-clauses clauses a)))))

(comment

  ;; we do we need to weave goals?

  (mplus g a0-inf a1-inf ...)

  ;; bind, just recur passing the subtitution to each goal
  ;; short-circuiting reduce

  ;; only choices should be placed onto the stream ?

  ;; goals create streams of choices

  ;; mzero is just nil on the stream
  ;; unit is (list a)
  ;; 

  ;; do we need to worry about holding onto the head?

  ;; wow lazy sequences are gonna make this whole system a lot simpler

  ;; where we would use inc, we just create a lazy-seq
  
  )

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