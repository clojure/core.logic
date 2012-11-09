(ns clojure.core.logic.bench
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :as l])
  (:require [clojure.core.logic.arithmetic :as a]
            [clojure.repl :as r]
            [clojure.pprint :as pp]
            [clojure.set :as set]))

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
;; cliques

(defrel connected ^:index x ^:index y)
(facts connected [[1 2] [1 5]])
(facts connected [[2 1] [2 3] [2 5]])
(facts connected [[3 2] [3 4]])
(facts connected [[4 3] [4 5] [4 6]])
(facts connected [[5 1] [5 2] [5 4]])
(facts connected [[6 4]])

(defne connected-to-allo
  "Ensure that vertex v is connected to all vertices
   vs."
  [v vs]
  ([_ ()])
  ([_ [vh . vr]]
     (connected v vh)
     (connected-to-allo v vr)))

(defne all-connected-to-allo
  "Collect all cliques in l. l must be bounded to ensure
   termination."
  [l]
  ([()])
  ([[h . t]]
     (connected-to-allo h t)
     (all-connected-to-allo t)))

(comment
  (run-nc* [q]
    (fresh [a b d]
      (== q (llist a b d))
      (bounded-listo q 6)
      (all-connected-to-allo q)))
  
  ;; 350-400ms
  (dotimes [_ 5]
    (time
     (dotimes [_ 100]
      (run-nc 20 [q]
        (fresh [a b d]
          (== q (llist a b d))
          (bounded-listo q 6)
          (all-connected-to-allo q))))))
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

(defn debug-doms []
  (fn [a]
    (let [s (:s a)]
      (pp/pprint
       (zipmap (keys s)
         (map (fn [v]
                (let [v (walk a v)]
                  (if (lvar? v)
                    (get-dom a v)
                    v)))
              (keys s)))))
    a))

(defn cryptarithfd-1 []
  (run* [q]
    (fresh [s e n d m o r y]
      (== q [s e n d m o r y])
      (infd s e n d m o r y (interval 0 9))
      (distinctfd q)
      (distribute q ::l/ff)
      (!=fd m 0) (!=fd s 0)
      (eqfd
        (=             (+ (* 1000 s) (* 100 e) (* 10 n) d
                          (* 1000 m) (* 100 o) (* 10 r) e)
           (+ (* 10000 m) (* 1000 o) (* 100 n) (* 10 e) y)))
      (debug-doms))))

;; Bratko 3rd ed pg 343

(defn cryptarithfd-2 []
  (run* [q]
    (fresh [d o n a l g e r b t]
      (== q [d o n a l g e r b t])
      (distribute q ::l/ff)
      (infd d o n a l g e r b t (interval 0 9))
      (distinctfd q)
      (eqfd
        (= (+ (* 100000 d) (* 10000 o) (* 1000 n) (* 100 a) (* 10 l) d
              (* 100000 g) (* 10000 e) (* 1000 r) (* 100 a) (* 10 l) d)
           (+ (* 100000 r) (* 10000 o) (* 1000 b) (* 100 e) (* 10 r) t))))))

(comment
  ;; FIXME: we don't see as much propagation as Oz, why not?

  (cryptarithfd-1)

  (time (cryptarithfd-1))

  ;; ~2700ms, a little bit slower w/ distribute step
  (dotimes [_ 5]
    (time
     (dotimes [_ 100] 
       (cryptarithfd-1))))

  ;; 3X slower still
  (dotimes [_ 5]
    (time
     (dotimes [_ 10] 
       (cryptarithfd-1))))

  ;; WORKS: takes a long time ([5 2 6 4 8 1 9 7 3 0])
  ;; 128797.253
  ;; 17.7s w/ distribute 7X faster
  (time (cryptarithfd-2))
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
  ;; close to 2X faster than Petite Chez
  ;; ~3500ms
  (dotimes [_ 5]
    (time
     (dotimes [_ 1000]
       (dinesmanfd))))

  (-> (dinesmanfd) first ->answer)  ; (:smith :cooper :baker :fletcher :miller)
  )

;; =============================================================================
;; Simple

(defn simplefd []
  (run* [q]
    (fresh [x y]
      (== q [x y])
      (infd x y (interval 0 9))
      (+fd x y 9)
      (fresh [p0 p1]
        (*fd 2 x p0)
        (*fd 4 y p1)
        (+fd p0 p1 24)))))

;; with eqfd sugar

(defn simple-eqfd []
  (run* [q]
    (fresh [x y]
      (infd x y (interval 0 9))
      (eqfd
        (= (+ x y) 9)
        (= (+ (* x 2) (* y 4)) 24))
      (== q [x y]))))

(comment
  ;; "Finite Domain Constraint Programming in Oz. A Tutorial." (Schulte & Smolka)
  ;; currently none of the constraints above trigger any refinements!
  (simplefd)

  (simple-eqfd)

  ;; 900ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e3] 
       (simple-eqfd))))
  
  (run* [q]
    (fresh [a b]
      (*fd a 3 34)
      (debug-doms)))
  )

;; =============================================================================
;; Stone Problem

;; w  - is a stone (weight)
;; sl - stone (weight) list
;; r  - the input range we can construct
;; o  - the output range we can construct
;; n  - the bound

(defne subchecko [w sl r o n]
  ;; we have no more stones to test in sl to test w with
  ([_ () _ _ _]
     (fresh [hr]
       (infd hr (interval 1 n))
       (matche [r o]
         ;; r is not empty, we add w to the output only if
         ;; w is head of r + 1
         ([[hr . _] [w . r]] (+fd hr 1 w))
         ;; r is empty, just add the weight
         ;; only works for w == 1
         ([() [w . r]]))))
  ;; we have stones to in sl to test w with
  ([_ [hsl . rsl] _ _ _]
     (fresh [w-hsl w+hsl o0 o1 nw]
       (infd hsl w-hsl w+hsl (interval 1 n))
       (+fd hsl w-hsl w) (+fd hsl w w+hsl)
       ;; attempt to construct values prior w
       (subchecko w-hsl rsl r  o0 n)
       ;; attempt to construct values around w
       (subchecko w     rsl o0 o1 n)
       ;; attempt to construct values after w
       (subchecko w+hsl rsl o1 o  n))))

;; checks that list of weight can produce the list
;; of integers from 1 to 40, in reverse order (40 ... 1)
(defne checko [ws sl r n]
  ;; if ws is empty, the first element of r must be n
  ([() _ [a . _] a])
  ;; otherwise we check the first weight
  ([[w . wr] _ _ _]
     (fresh [nsl nr]
       ;; check the first weight with subchecko
       (subchecko w sl r nr n)
       ;; if it succeeds we add w to the new stone list
       (conso w sl nsl)
       ;; check remaining weights
       (checko wr nsl nr n))))

(defn matches [n]
  (run 1 [q]
    (fresh [a b c d s1 s2]
      (infd a b c d s1 s2 (interval 1 n)) 
      (distinctfd [a b c d])
      (== a 1)
      (<=fd a b) (<=fd b c) (<=fd c d)
      (eqfd (= (+ a b c d) n))
      (checko [a b c d] () () n)
      (== q [a b c d]))))

(comment
  (time (matches 40))

  ;; ~8200ms
  (dotimes [_ 5]
    (time
     (dotimes [_ 1000]
       (matches 40))))
  )

;; =============================================================================
;; Sudoku

;; -----------------------------------------------------------------------------
;; small

(defn small-sudokufd []
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
        (everyg distinctfd
           [row1 row2 row3 row4
            col1 col2 col3 col4
            sq1 sq2 sq3 sq4])))))

(comment
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e3] 
       (small-sudokufd))))

  (small-sudokufd)
  )

;; -----------------------------------------------------------------------------
;; 9x9

(defn get-square [rows x y]
  (for [x (range x (+ x 3))
        y (range y (+ y 3))]
    (get-in rows [x y])))

(defn init [vars hints]
  (if (seq vars)
    (let [hint (first hints)]
      (all
       (if-not (zero? hint)
         (== (first vars) hint)
         succeed)
       (init (next vars) (next hints))))
    succeed))

(defn ->rows [xs]
  (->> xs (partition 9) (map vec) (into [])))

(defn ->cols [rows]
  (apply map vector rows))

(defn ->squares [rows]
  (for [x (range 0 9 3)
        y (range 0 9 3)]
    (get-square rows x y)))

(defn sudokufd [hints]
  (let [vars (repeatedly 81 lvar) 
        rows (->rows vars)
        cols (->cols rows)
        sqs  (->squares rows)]
    (run-nc 1 [q]
      (== q vars)
      (distribute q ::l/ff)
      (everyg #(infd % (domain 1 2 3 4 5 6 7 8 9)) vars)
      (init vars hints)
      (everyg distinctfd rows)
      (everyg distinctfd cols)
      (everyg distinctfd sqs))))

;; Helpers

(defn verify [vars]
  (let [rows (->rows vars)
        cols (->cols rows)
        sqs  (->squares rows)
        verify-group (fn [group]
                       (every? #(= (->> % (into #{}) count) 9)
                          group))]
    (and (verify-group rows)
         (verify-group cols)
         (verify-group sqs))))

(defn print-solution [vars]
  (doseq [row-group (->> vars
                        (partition 9)
                        (partition 3)
                        (interpose "\n\n"))]
    (if-not (string? row-group)
      (doseq [row (interpose "\n" row-group)]
        (if-not (string? row)
          (doseq [x (->> row
                         (partition 3)
                         (map #(interpose " " %))
                         (interpose "  "))]
            (print (apply str x)))
          (print row)))
      (print row-group)))
  (println) (println))

(comment
  (def easy0
    [0 0 3  0 2 0  6 0 0
     9 0 0  3 0 5  0 0 1
     0 0 1  8 0 6  4 0 0

     0 0 8  1 0 2  9 0 0
     7 0 0  0 0 0  0 0 8
     0 0 6  7 0 8  2 0 0

     0 0 2  6 0 9  5 0 0
     8 0 0  2 0 3  0 0 9
     0 0 5  0 1 0  3 0 0])

  (def easy1
    [2 0 0  0 8 0  3 0 0
     0 6 0  0 7 0  0 8 4
     0 3 0  5 0 0  2 0 9

     0 0 0  1 0 5  4 0 8
     0 0 0  0 0 0  0 0 0
     4 0 2  7 0 6  0 0 0

     3 0 1  0 0 7  0 4 0
     7 2 0  0 4 0  0 6 0
     0 0 4  0 1 0  0 0 3])
  
  (sudokufd easy0)
  (time (sudokufd easy0))
  
  (sudokufd easy1)
  (time (sudokufd easy1))

  (-> (sudokufd easy0) first print-solution)

  (-> (sudokufd easy0) first verify)

  ;; ~2250ms
  (dotimes [_ 5]
    (time
     (dotimes [_ 100]
       (sudokufd easy0))))

  ;; ~2800ms
  (dotimes [_ 5]
    (time
     (dotimes [_ 100]
       (sudokufd easy1))))

  ;; Hardest Norvig Random
  (def hard0
    [0 0 0  0 0 6  0 0 0
     0 5 9  0 0 0  0 0 8
     2 0 0  0 0 8  0 0 0

     0 4 5  0 0 0  0 0 0
     0 0 3  0 0 0  0 0 0
     0 0 6  0 0 3  0 5 4

     0 0 0  3 2 5  0 0 6
     0 0 0  0 0 0  0 0 0
     0 0 0  0 0 0  0 0 0])

  ;; ~14.2s
  ;; this one behaves very badly w/ distribute
  ;; it would be very interesting to determine why
  (time (sudokufd hard0))

  (-> (sudokufd hard0) first verify)

  (dotimes [_ 5]
    (time
     (dotimes [_ 1]
       (sudokufd hard0))))

  ;; from GeCode test suite
  (def hard1
    [0 0 0  0 0 3  0 6 0
     0 0 0  0 0 0  0 1 0
     0 9 7  5 0 0  0 8 0

     0 0 0  0 9 0  2 0 0
     0 0 8  0 7 0  4 0 0
     0 0 3  0 6 0  0 0 0
     
     0 1 0  0 0 2  8 9 0
     0 4 0  0 0 0  0 0 0
     0 5 0  1 0 0  0 0 0])

  ;; ~900ms
  (time (sudokufd hard1))

  (-> (sudokufd hard1) first verify)

  ;; 8.7s seconds w/o distribute
  ;; < 800ms w/ distribute, 10X faster
  (dotimes [_ 5]
    (time
     (dotimes [_ 10]
       (sudokufd hard1))))

  ;; from Wikipedia
  (def hard2
    [1 2 0  4 0 0  3 0 0
     3 0 0  0 1 0  0 5 0
     0 0 6  0 0 0  1 0 0

     7 0 0  0 9 0  0 0 0
     0 4 0  6 0 3  0 0 0
     0 0 3  0 0 2  0 0 0

     5 0 0  0 8 0  7 0 0
     0 0 7  0 0 0  0 0 5
     0 0 0  0 0 0  0 9 8])

  ;; ~3.5s
  ;; 900ms w/ distribute
  (time (sudokufd hard2))

  (-> (sudokufd hard2) first print-solution)

  (dotimes [_ 5]
    (time
     (sudokufd hard2)))
  )

;; From "Finite Domain Constraint Programming in Oz. A Tutorial" pg 22

(defn safefd []
  (run* [q]
    (fresh [c1 c2 c3 c4 c5 c6 c7 c8 c9]
      (infd c1 c2 c3 c4 c5 c6 c7 c8 c9 (interval 1 9))
      (== q [c1 c2 c3 c4 c5 c6 c7 c8 c9])
      (distinctfd q)
      (eqfd
        (= (- c4 c6) c7)
        (= (* c1 c2 c3) (+ c8 c9))
        (< (+ c2 c3 c6) c8)
        (< c9 c8))
      (!=fd c1 1) (!=fd c2 2) (!=fd c3 3)
      (!=fd c4 4) (!=fd c5 5) (!=fd c6 6)
      (!=fd c7 7) (!=fd c8 8) (!=fd c9 9))))

(comment
  ;; FIXME: see a disjoint error now

  (time (safefd))

  ;; FIXME: adding (distribute q ::l/ff) causes an exception

  (every?
    (fn [[c1 c2 c3 c4 c5 c6 c7 c8 c9]]
      (and
        (not= c1 1) (not= c2 2) (not= c3 3)
        (not= c4 4) (not= c5 5) (not= c6 6)
        (not= c7 7) (not= c8 8) (not= c9 9)
        (= (- c4 c6) c7)
        (= (* c1 c2 c3) (+ c8 c9))
        (< (+ c2 c3 c6) c8)
        (< c9 c8)))
    (safefd))

  ;; 2800ms
  ;; also significantly slower
  (dotimes [_ 5]
    (time
     (dotimes [_ 10] 
       (safefd))))
  )