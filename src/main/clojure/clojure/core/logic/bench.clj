(ns clojure.core.logic.bench
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.arithmetic :as a]
            [clojure.repl :as r]
            [clojure.pprint :as pp]))

(comment
  (run* [q]
    (== q true))

  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (run* [q]
         (== q true)))))
 )

(comment
  (run 1 [q]
    (fresh [x y]
      (appendo x y q)))

  ;; 453ms
  (dotimes [_ 5]
    (time
     (dotimes [_ 1]
       (run 700 [q]
         (fresh [x y]
           (appendo x y q))))))
  )

;; =============================================================================
;; nrev
;; =============================================================================

(defne nrevo [l o]
  ([() ()])
  ([[a . d] _]
     (fresh [r]
       (nrevo d r)
       (appendo r [a] o))))

(comment
  ;; we can run backwards, unlike Prolog
  (run 1 [q] (nrevo q (range 30)))

  ;; SWI-Prolog 0.06-0.08s
  ;; ~3.7s
  (let [data (into [] (range 30))]
    (binding [*occurs-check* false]
      (dotimes [_ 5]
        (time
         (dotimes [_ 1e3]
           (run 1 [q] (nrevo data q)))))))

  ;; the LIPS are ridiculously high for SWI-Prolog
  ;; clearly nrev is a case that SWI-Prolog can optimize away
  )

;; =============================================================================
;; zebra
;; =============================================================================

(defne righto [x y l]
  ([_ _ [x y . r]])
  ([_ _ [_ . r]] (righto x y r)))

(defn nexto [x y l]
  (conde
    [(righto x y l)]
    [(righto y x l)]))

(defn zebrao [hs]
  (all
   (== [(lvar) (lvar) [(lvar) (lvar) 'milk (lvar) (lvar)] (lvar) (lvar)] hs)
   (firsto hs ['norwegian (lvar) (lvar) (lvar) (lvar)])
   (nexto ['norwegian (lvar) (lvar) (lvar) (lvar)] [(lvar) (lvar) (lvar) (lvar) 'blue] hs)
   (righto [(lvar) (lvar) (lvar) (lvar) 'ivory] [(lvar) (lvar) (lvar) (lvar) 'green] hs)
   (membero ['englishman (lvar) (lvar) (lvar) 'red] hs)
   (membero [(lvar) 'kools (lvar) (lvar) 'yellow] hs)
   (membero ['spaniard (lvar) (lvar) 'dog (lvar)] hs)
   (membero [(lvar) (lvar) 'coffee (lvar) 'green] hs)
   (membero ['ukrainian (lvar) 'tea (lvar) (lvar)] hs)
   (membero [(lvar) 'lucky-strikes 'oj (lvar) (lvar)] hs)
   (membero ['japanese 'parliaments (lvar) (lvar) (lvar)] hs)
   (membero [(lvar) 'oldgolds (lvar) 'snails (lvar)] hs)
   (nexto [(lvar) (lvar) (lvar) 'horse (lvar)] [(lvar) 'kools (lvar) (lvar) (lvar)] hs)
   (nexto [(lvar) (lvar) (lvar) 'fox (lvar)] [(lvar) 'chesterfields (lvar) (lvar) (lvar)] hs)))

(comment
  (run 1 [q] (zebrao q))
  
  ;; SWI-Prolog 6-8.5s
  ;; ~2.4s
  (binding [*occurs-check* false]
    (dotimes [_ 5]
      (time
       (dotimes [_ 1e3]
         (run 1 [q] (zebrao q))))))

  ;; ~3.7s
  (dotimes [_ 5]
    (time
     (dotimes [_ 1e3]
       (run 1 [q] (zebrao q)))))
  )

;; =============================================================================
;; nqueens

;; Bratko 3d pg 103

(comment
  (declare noattacko)

 (defne nqueenso [l]
   ([()])
   ([[[x y] . others]]
      (nqueenso others)
      (membero y [1 2 3 4 5 6 7 8])
      (noattacko [x y] others)))

 (defne noattacko [q others]
   ([_ ()])
   ([[x y] [[x1 y1] . r]]
      (!= y y1)
      (project [y y1 x x1]
        (!= (- y1 y) (- x1 x))
        (!= (- y1 y) (- x x1)))
      (noattacko [x y] r)))

 (defn solve-nqueens []
   (run* [q]
     (fresh [y1 y2 y3 y4 y5 y6 y7 y8]
       (== q [[1 y1] [2 y2] [3 y3] [4 y4] [5 y5] [6 y6] [7 y7] [8 y8]])
       (nqueenso q))))
 )

(comment
  (take 1 (solve-nqueens))

  ;; 92 solutions
  (count (solve-nqueens))

  ;; < 3s for 100x
  ;; about 18X slower that SWI
  (binding [*occurs-check* false]
    (dotimes [_ 5]
      (time
       (dotimes [_ 1]
         (take 1 (solve-nqueens))))))

  ;; ~550ms
  (binding [*occurs-check* false]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1]
         (solve-nqueens)))))

  ;; ~610ms
  (dotimes [_ 10]
    (time
     (binding [*occurs-check* false]
      (dotimes [_ 1]
        (solve-nqueens)))))

  ;; nqueens benefits from constraints
  )

;; =============================================================================
;; nqueensfd

;; based on Bratko 3d pg 344, constraint version

(comment
  ;; direct translation does not work
  ;; because of the subtraction constraints
  ;; also, some domain inference would be nice
  
  (defne noattackfd [y ys d]
    ([_ () _])
    ([y1 [y2 . yr] d]
       (fresh [x nd]
         (infd x nd (interval 1 8))
         (!=fd d x)
         (conde
           [(<fd y1 y2) (+fd y1 x y2)]
           [(<fd y2 y1) (+fd y2 x y1)])
         (+fd d 1 nd)
         (noattackfd y1 yr nd))))

  (defne safefd [l]
    ([()])
    ([[y . ys]]
       (noattackfd y ys 1)
       (safefd ys)))

  (defn nqueensfd []
    (run* [q]
      (fresh [a b c d e f g h]
        (infd a b c d e f g h (interval 1 8))
        (== q [a b c d e f g h])
        (distinctfd q)
        (safefd q))))

  (nqueensfd)

  (run* [q]
    (fresh [a b]
      (infd a b (interval 1 8))
      (== q [a b])
      (safefd q)))
  )

;; =============================================================================
;; send more money

(defne takeouto [x l y]
  ([_ [x . y] _])
  ([_ [h . t] [h . r]] (takeouto x t r)))

(defn digito [x l y]
  (takeouto x l y))

(defn first-digito [x l y]
  (all
   (digito x l y)
   (a/> x 0)))

(defne do-send-moolao [q l ll]
  ([[send more money] _ _]
     (fresh [s e n d m o r y
             l1 l2 l3 l4 l5 l6 l7 l8 l9]
       (first-digito s l l1)
       (first-digito m l1 l2)
       (digito e l2 l3)
       (digito n l3 l4)
       (digito d l4 l5)
       (digito o l5 l6)
       (digito r l6 l7)
       (digito y l7 l8)
       (project [s e n d m o r y]
         (== send (+ (* s 1000) (* e 100) (* n 10) d))
         (== more (+ (* m 1000) (* o 100) (* r 10) e))
         (== money (+ (* m 10000) (* o 1000) (* n 100) (* e 10) y))
         (project [send more]
           (== money (+ send more)))))))

(defn send-money-quicklyo [send more money]
  (fresh [l]
    (do-send-moolao [send more money] (range 10) l)))

(comment
  ;; ~16-17s, w/o occurs-check
  ;; SWI-Prolog takes 4s, so 3.8X faster
  ;; again I suspect the overhead here is from
  ;; interleaving, need to figure
  (time
   (binding [*occurs-check* false]
     (run 1 [q]
       (fresh [send more money]
         (send-money-quicklyo send more money)
         (== [send more money] q)))))
  )

;; =============================================================================
;; Cryptarithmetic Puzzle
;; Bratko 3rd ed pg 343

;; TODO: we want sugar for this

#_(defn cryptarithfd []
  (run* [d o n a l d
         g e r a l d
         r o b e r t] :as q
    (infd q (interval 0 9))
    (distintfd q)
    (eqfd
     (= (+ (* 100000 d) (* 10000 o) (* 1000 n) (* 100 a) (* 10 l) d
           (* 100000 g) (* 10000 e) (* 1000 r) (* 100 a) (* 10 l) d)
        (+ (* 100000 r) (* 10000 o) (* 1000 b) (* 100 e) (* 10 r) t)))))

(comment
  ;; eqfd, codewalking convenience macro

  (eqfd
   (= (+ (* 100000 d) (* 10000 o) (* 1000 n) (* 100 a) (* 10 l) d
         (* 100000 g) (* 10000 e) (* 1000 r) (* 100 a) (* 10 l) d)
      (+ (* 100000 r) (* 10000 o) (* 1000 b) (* 100 e) (* 10 r) t)))
  )

;; =============================================================================
;; Hanoi

(defne moveo [n x y z]
  ([1 _ _ _]
     (trace-lvars "Move top disk from " x)
     (trace-lvars " to " y))
  ([_ _ _ _]
     (pred n #(> % 1))
     (fresh [m _] (is m n dec)
       (moveo m x z y) (moveo 1 x y _) (moveo m z y x))))

(comment
  (run* [q]
    (moveo 3 :left :right :center))
  )

;; =============================================================================
;; Quick Sort

(declare partitiono)

(defne qsorto [l r r0]
  ([[] _ r])
  ([[x . lr] _ _]
     (fresh [l1 l2 r1]
       (partitiono lr x l1 l2)
       (qsorto l2 r1 r0)
       (qsorto l1 r (lcons x r1)))))

(defne partitiono [a b c d]
  ([[x . l] _ [x . l1] _]
     (conda
       ((project [x b]
          (== (<= x b) true))
        (partition l b l1 d))
       (partition l b c d))))

;; =============================================================================
;; Dinesman Dwelling Problem with CLP(FD)

(defn not-adjacento [x y]
  (fresh [f]
    (infd f (interval 1 5))
    (conde
      [(+fd x f y) (<fd 1 f)]
      [(+fd y f x) (<fd 1 f)])))

(defn dinesmanfd []
  (run* [q]
    (fresh [baker cooper fletcher miller smith]
      (== q [baker cooper fletcher miller smith])
      (distinctfd [baker cooper fletcher miller smith])
      (infd baker cooper fletcher miller smith (interval 1 5))
      (!=fd baker 5) (!=fd cooper 1)
      (!=fd fletcher 5) (!=fd fletcher 1)
      (<fd cooper miller) 
      (not-adjacento smith fletcher)
      (not-adjacento fletcher cooper))))

(defn sort-dwellers [[fa _] [fb _]]
  (cond (< fa fb) -1 (= fa fb) 0 :else 1))

(defn ->answer [ns]
  (->> (map vector ns [:baker :cooper :fletcher :miller :smith])
       (sort sort-dwellers)
       (map second)))

(comment
  ;; ~2250ms
  ;; close to 2X faster than Petite Chez
  (dotimes [_ 5]
    (time
     (dotimes [_ 1000]
       (dinesmanfd))))

  (-> (dinesmanfd) first ->answer)  ; (:smith :cooper :baker :fletcher :miller)
  )

;; =============================================================================
;; Stone Problem

(defne subchecko [w sl r o n]
  ([_ () _ _ _]
     (fresh [a]
       (infd a (interval 1 n))
       (matche [r o]
         ([[a . d] [w . r]] (+fd a 1 w))
         ([() [w . r]]))))
  ([_ [a . nsl] _ _ _]
     (fresh [b c ro0 ro1 nw]
       (infd a b c (interval 1 n))
       (+fd a b w) (+fd a w c)
       (subchecko b nsl r ro0 n)
       (subchecko w nsl ro0 ro1 n)
       (subchecko c nsl ro1 o n))))

(defne checko [ws sl r n]
  ([() _ [a . _] a])
  ([[w . wr] _ _ _]
     (fresh [nsl nr]
       (subchecko w sl r nr n)
       (conso w sl nsl)
       (checko wr nsl nr n))))

(defn matches [n]
  (run* [q]
    (fresh [a b c d s1 s2]
      (infd a b c d s1 s2 (interval 1 n)) 
      (distinctfd [a b c d])
      (== a 1)
      (<=fd a b) (<=fd b c) (<=fd c d)
      (+fd a b s1) (+fd s1 c s2) (+fd s2 d n)
      (checko [a b c d] () () n)
      (== q [a b c d]))))

(comment
  (time (matches 40))
  
  ;; ~2970ms much faster
  ;; so >2X faster than cKanren under Petite Chez
  (dotimes [_ 5]
    (time
     (dotimes [_ 1000]
       (matches 40))))
  )

;; =============================================================================
;; Sudoku

(defn distincto [s]
  (if s
    (all
     (distinctfd (first s))
     (distincto (next s)))
    s#))

(defn sudokufd []
  (run-nc 1 [q]
    (fresh [a1 a2 a3 a4
            b1 b2 b3 b4
            c1 c2 c3 c4
            d1 d2 d3 d4]
      (== q [[a1 a2 a3 a4]
             [b1 b2 b3 b4]
             [c1 c2 c3 c4]
             [d1 d2 d3 d4]])
      (infd a1 a2 a3 a4
            b1 b2 b3 b4
            c1 c2 c3 c4
            d1 d2 d3 d4
            (domain 1 2 3 4))
      (let [row1 [a1 a2 a3 a4]
            row2 [b1 b2 b3 b4]
            row3 [c1 c2 c3 c4]
            row4 [d1 d2 d3 d4]
            col1 [a1 b1 c1 d1]
            col2 [a2 b2 c2 d2]
            col3 [a3 b3 c3 d3]
            col4 [a4 b4 c4 d4]
            sq1 [a1 a2 b1 b2]
            sq2 [a3 a4 b3 b4]
            sq3 [c1 c2 d1 d2]
            sq4 [c3 c4 d3 d4]]
        (distincto [row1 row2 row3 row4
                    col1 col2 col3 col4
                    sq1 sq2 sq3 sq4])))))

(comment
  ;; ~1950ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e3] 
       (sudokufd))))
  )

(defn big-sudokufd []
  (run-nc 1 [q]
    (fresh [a1 a2 a3 a4 a5 a6 a7 a8 a9
            b1 b2 b3 b4 b5 b6 b7 b8 b9
            c1 c2 c3 c4 c5 c6 c7 c8 c9
            d1 d2 d3 d4 d5 d6 d7 d8 d9
            e1 e2 e3 e4 e5 e6 e7 e8 e9
            f1 f2 f3 f4 f5 f6 f7 f8 f9
            g1 g2 g3 g4 g5 g6 g7 g8 g9
            h1 h2 h3 h4 h5 h6 h7 h8 h9
            i1 i2 i3 i4 i5 i6 i7 i8 i9]
      (== q [[a1 a2 a3 a4 a5 a6 a7 a8 a9]
             [b1 b2 b3 b4 b5 b6 b7 b8 b9]
             [c1 c2 c3 c4 c5 c6 c7 c8 c9]
             [d1 d2 d3 d4 d5 d6 d7 d8 d9]
             [e1 e2 e3 e4 e5 e6 e7 e8 e9]
             [f1 f2 f3 f4 f5 f6 f7 f8 f9]
             [g1 g2 g3 g4 g5 g6 g7 g8 g9]
             [h1 h2 h3 h4 h5 h6 h7 h8 h9]
             [i1 i2 i3 i4 i5 i6 i7 i8 i9]])
      (infd a1 a2 a3 a4 a5 a6 a7 a8 a9
            b1 b2 b3 b4 b5 b6 b7 b8 b9
            c1 c2 c3 c4 c5 c6 c7 c8 c9
            d1 d2 d3 d4 d5 d6 d7 d8 d9
            e1 e2 e3 e4 e5 e6 e7 e8 e9
            f1 f2 f3 f4 f5 f6 f7 f8 f9
            g1 g2 g3 g4 g5 g6 g7 g8 g9
            h1 h2 h3 h4 h5 h6 h7 h8 h9
            i1 i2 i3 i4 i5 i6 i7 i8 i9
            (domain 1 2 3 4 5 6 7 8 9))
      (let [row1 [a1 a2 a3 a4 a5 a6 a7 a8 a9]
            row2 [b1 b2 b3 b4 b5 b6 b7 b8 b9]
            row3 [c1 c2 c3 c4 c5 c6 c7 c8 c9]
            row4 [d1 d2 d3 d4 d5 d6 d7 d8 d9]
            row5 [e1 e2 e3 e4 e5 e6 e7 e8 e9]
            row6 [f1 f2 f3 f4 f5 f6 f7 f8 f9]
            row7 [g1 g2 g3 g4 g5 g6 g7 g8 g9]
            row8 [h1 h2 h3 h4 h5 h6 h7 h8 h9]
            row9 [i1 i2 i3 i4 i5 i6 i7 i8 i9]
            col1 [a1 b1 c1 d1 e1 f1 g1 h1 i1]
            col2 [a2 b2 c2 d2 e2 f2 g2 h2 i2]
            col3 [a3 b3 c3 d3 e3 f3 g3 h3 i3]
            col4 [a4 b4 c4 d4 e4 f4 g4 h4 i4]
            col5 [a5 b5 c5 d5 e5 f5 g5 h5 i5]
            col6 [a6 b6 c6 d6 e6 f6 g6 h6 i6]
            col7 [a7 b7 c7 d7 e7 f7 g7 h7 i7]
            col8 [a8 b8 c8 d8 e8 f8 g8 h8 i8]
            col9 [a9 b9 c9 d9 e9 f9 g9 h9 i9]
            sq1 [a1 a2 a3 b1 b2 b3 c1 c2 c3]
            sq2 [a4 a5 a6 b4 b5 b6 c4 c5 c6]
            sq3 [a7 a8 a9 b7 b8 b9 c7 c8 c9]
            sq4 [d1 d2 d3 e1 e2 e3 f1 f2 f3]
            sq5 [d4 d5 d6 e4 e5 e6 f4 f5 f6]
            sq6 [d7 d8 d9 e7 e8 e9 f7 f8 f9]
            sq7 [g1 g2 g3 h1 h2 h3 i1 i2 i3]
            sq8 [g4 g5 g6 h4 h5 h6 i4 i5 i6]
            sq9 [g7 g8 g9 h7 h8 h9 i7 i8 i9]]
        (distincto [row1 row2 row3 row4 row5 row6 row7 row8 row9
                    col1 col2 col3 col4 col5 col6 col7 col8 col9
                    sq1 sq2 sq3 sq4 sq5 sq6 sq7 sq8 sq9])))))

(comment
  ;; 2.2s w/ interval
  (dotimes [_ 5]
    (time (big-sudokufd)))
  ;; 1.4s w/ domain

  ;; this is 1000X slower than the small sudoku

  (big-sudokufd)

  ;; 27875 calls to distinct fd
  ;; 137249 calls to process-dom
  ;; 141053 calls to FD difference

  ;; these numbers seem high
  )