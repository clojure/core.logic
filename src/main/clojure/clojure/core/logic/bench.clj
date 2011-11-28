(ns clojure.core.logic.bench
  (:refer-clojure :exclude [reify inc ==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.arithmetic :as a]))

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
  
  ;; 1.4s
  (dotimes [_ 10]
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
  ([[?a . ?d] _]
     (fresh [r]
       (nrevo ?d r)
       (appendo r [?a] o))))

(comment
  ;; we can run backwards, unlike Prolog
  (run 1 [q] (nrevo q (range 30)))

  ;; SWI-Prolog 0.06-0.08s
  ;; ~4.1s
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
  ([_ _ [x y . ?r]])
  ([_ _ [_ . ?r]] (righto x y ?r)))

(defn nexto [x y l]
  (conde
    [(righto x y l)]
    [(righto y x l)]))

(defn zebrao [hs]
  (all
   (== [(lvar) (lvar) [(lvar) (lvar) 'milk (lvar) (lvar)] (lvar) (lvar)] hs)                          (firsto hs ['norwegian (lvar) (lvar) (lvar) (lvar)])                         
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
  ;; SWI-Prolog 6-8.5s
  ;; ~2.4s
  (binding [*occurs-check* false]
    (dotimes [_ 5]
      (time
       (dotimes [_ 1e3]
         (run 1 [q] (zebrao q))))))

  ;; < 3s
  (dotimes [_ 5]
    (time
     (dotimes [_ 1e3]
       (run 1 [q] (zebrao q)))))
  )

;; =============================================================================
;; nqueens

;; Bratko pg 103

(declare noattacko)

(defne nqueenso [l]
  ([()])
  ([[[?x ?y] . ?others]]
     (nqueenso ?others)
     (membero ?y [1 2 3 4 5 6 7 8])
     (noattacko [?x ?y] ?others)))

(defne noattacko [q others]
  ([_ ()])
  ([[?x ?y] [[?x1 ?y1] . ?others]]
     (!= ?y ?y1)
     (project [?y ?y1 ?x ?x1]
       (!= (- ?y1 ?y) (- ?x1 ?x))
       (!= (- ?y1 ?y) (- ?x ?x1)))
     (noattacko [?x ?y] ?others)))

(defn solve-nqueens []
  (run* [q]
    (fresh [y1 y2 y3 y4 y5 y6 y7 y8]
      (== q [[1 y1] [2 y2] [3 y3] [4 y4] [5 y5] [6 y6] [7 y7] [8 y8]])
      (nqueenso q))))

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
     (dotimes [_ 1]
       (solve-nqueens))))

  ;; nqueens benefits from constraints
  )

;; Bratko pg 344, constraint version

;; =============================================================================
;; send more money

(defne takeouto [x l y]
  ([_ [x . y] _])
  ([_ [?h . ?t] [?h . ?r]] (takeouto x ?t ?r)))
 
(defn digito [x l y]
  (takeouto x l y))
  
(defn first-digito [x l y]
  (all
   (digito x l y)
   (a/> x 0)))

(defne do-send-moolao [q l ll]
  ([[?send ?more ?money] _ _]
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
         (== ?send (+ (* s 1000) (* e 100) (* n 10) d))
         (== ?more (+ (* m 1000) (* o 100) (* r 10) e))
         (== ?money (+ (* m 10000) (* o 1000) (* n 100) (* e 10) y))
         (project [?send ?more]
           (== ?money (+ ?send ?more)))))))

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
;; Quick Sort

(declare partitiono)

(defne qsorto [l r r0]
  ([[] _ r])
  ([[?x . ?lr] _ _]
     (fresh [l1 l2 r1]
       (partitiono ?lr ?x l1 l2)
       (qsorto l2 r1 r0)
       (qsorto l1 r (lcons ?x r1)))))

(defne partitiono [a b c d]
  ([[?x . ?l] _ [?x . ?l1] _]
     (conda
       ((project [?x b]
          (== (<= ?x b) true))
        (partition ?l b ?l1 d))
       (partition ?l b c d))))