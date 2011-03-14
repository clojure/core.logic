(ns logos.arithmetic
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren)
  (:use [clojure.pprint :only [pprint]]))

(defn bit-xor-o [x y r]
  (cond-e
   ((== x 0) (== 0 y) (== 0 r))
   ((== 1 x) (== 0 y) (== 1 r))
   ((== 0 x) (== 1 y) (== 1 r))
   ((== 1 x) (== 1 y) (== 0 r))))

(defn bit-and-o [x y r]
  (cond-e
   ((== 0 x) (== 0 y) (== 0 r))
   ((== 1 x) (== 0 y) (== 0 r))
   ((== 0 x) (== 1 y) (== 0 r))
   ((== 1 x) (== 1 y) (== 1 r))))

(defn half-adder-o [x y r c]
  (all
   (bit-xor-o x y r)
   (bit-and-o x y c)))

(defn full-adder-o [b x y r c]
  (exist [w xy wz]
    (half-adder-o x y w xy)
    (half-adder-o w b r wz)
    (bit-xor-o xy wz c)))

(defn build-num [n]
  (cond
   (zero? n) '()
   (and (not (zero? n))
        (even? n))
     (cons 0 (build-num (/ n 2)))
   (odd? n)
     (cons 1 (build-num (/ (- n 1) 2)))))

(defn pos-o [n]
  (exist [a d]
    (== (lcons a d) n)))

(defn >1-o [n]
  (exist [a ad dd]
    (== (llist a ad dd) n)))

(declare adder-o)

(defn gen-adder-o [d n m r]
  (exist [a b c x y z]
    (== (lcons a x) n)
    (== (lcons b y) m)
    (== (lcons c z) r)
    (exist [e]
      (pos-o y) (pos-o z)
      (full-adder-o d a b c e)
      (adder-o e x y z))))

(defn adder-o [d n m r]
  (cond-e
   ((== 0 d) (== '() m) (== n r))
   ((== 0 d) (== '() n) (== m r) (pos-o m))
   ((== 1 d) (== '() m) (adder-o 0 n '(1) r))
   ((== 1 d) (== '() n) (pos-o m) (adder-o 0 '(1) m r))
   ((== '(1) n) (== '(1) m)
    (exist [a c]
           (== `(~a ~c) r)
           (full-adder-o d 1 1 a c)))
   ((== '(1) n) (gen-adder-o d n m r))
   ((== '(1) m) (>1-o n) (>1-o r) (adder-o d '(1) n r))
   ((>1-o n) (gen-adder-o d n m r))))

(defn plus-o [n m k]
  (adder-o 0 n m k))

(comment
  ;; ()
  (run* [q]
    (exist [n]
      (pos-o '())
        (== q true)))

  ;; ((_.0 . _.1))
  (run* [q]
    (pos-o q))

  ;; (true)
  (run* [q]
    (>1-o '(0 1))
    (== true q))

  ;; ()
  (run* [q]
    (>1-o '(1))
    (== true q))

  ;; ((_.0 _.1 . _.2))
  (run* [q]
    (>1-o q))

  ;; ((0 1 0 1))
  (run* [s]
    (gen-adder-o 1 '(0 1 1) '(1 1) s))

  (pprint
   (run 9 [q]
        (exist [x y r]
               (plus-o x y r)
               (== [x y r] q))))

  ;; 100ms
  (dotimes [_ 5]
    (time
     (dotimes [_ 1]
       (doall
        (run 500 [q]
             (exist [x y r]
                    (plus-o x y r)
                    (== [x y r] q)))))))

  ;; we do see duplicate results if n is > 7
  ;; however it's not clear to me if this is because
  ;; the implementation of mK is different from TRS
  (run 6 [s]
        (exist [x y]
               (adder-o 0 x y '(1 0 1))
               (== `(~x ~y) s)))
  )