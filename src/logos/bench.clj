(ns logos.bench
  (:refer-clojure :exclude [reify inc ==])
  (:use [logos minikanren match]
        [logos.logic :only [first-o member-o]]
        [logos.disequality :only [!=]])
  (:require [logos.nonrel :as nonrel]
            [clojure.contrib.macro-utils :as macro]))

;; =============================================================================
;; nrev
;; =============================================================================

(defn-e append-o [x y z]
  ([() _ y])
  ([[?a . ?d] _ [?a . ?r]] (append-o ?d y ?r)))

(defn-e nrev-o [l o]
  ([() ()])
  ([[?a . ?d] _]
     (exist [r]
      (nrev-o ?d r)
      (append-o r [?a] o))))

(comment
  ;; we can run backwards, unlike Prolog
  (run 1 [q] (nrev-o q (range 30)))

  ;; SWI-Prolog 0.06-0.08s
  ;; ~4.1s
  (let [data (into [] (range 30))]
    (binding [*occurs-check* false]
     (dotimes [_ 5]
       (time
        (dotimes [_ 1e3]
          (doall (run 1 [q] (nrev-o data q))))))))

  ;; the LIPS are ridiculously high for SWI-Prolog
  ;; clearly nrev is a case that SWI-Prolog can optimize away
  )

;; =============================================================================
;; zebra
;; =============================================================================

(defn-e on-right-o [x y l]
  ([_ _ [x y . ?r]])
  ([_ _ [_ . ?r]] (on-right-o x y ?r)))

(defn next-to-o [x y l]
  (cond-e
   ((on-right-o x y l))
   ((on-right-o y x l))))

(defn zebra-o [hs]
  (macro/symbol-macrolet [_ (lvar)]
   (all
    (== [_ _ [_ _ 'milk _ _] _ _] hs)                         
    (first-o hs ['norwegian _ _ _ _])                         
    (next-to-o ['norwegian _ _ _ _] [_ _ _ _ 'blue] hs)       
    (on-right-o [_ _ _ _ 'ivory] [_ _ _ _ 'green] hs)         
    (member-o ['englishman _ _ _ 'red] hs)                    
    (member-o [_ 'kools _ _ 'yellow] hs)                      
    (member-o ['spaniard _ _ 'dog _] hs)                      
    (member-o [_ _ 'coffee _ 'green] hs)                      
    (member-o ['ukrainian _ 'tea _ _] hs)                     
    (member-o [_ 'lucky-strikes 'oj _ _] hs)                  
    (member-o ['japanese 'parliaments _ _ _] hs)              
    (member-o [_ 'oldgolds _ 'snails _] hs)                   
    (next-to-o [_ _ _ 'horse _] [_ 'kools _ _ _] hs)          
    (next-to-o [_ _ _ 'fox _] [_ 'chesterfields _ _ _] hs))))

(comment
  ;; SWI-Prolog 6-8.5s
  ;; < 2.1s
  ;; with metadata checks ~2.2
  (binding [*occurs-check* false]
   (dotimes [_ 5]
    (time
     (dotimes [_ 1e3]
       (doall (run 1 [q] (zebra-o q)))))))

  ;; < 3s
  (dotimes [_ 5]
    (time
     (dotimes [_ 1e3]
       (doall (run 1 [q] (zebra-o q))))))
  )

;; =============================================================================
;; nqueens

;; Bratko pg 103 on, nqueens variants

;; first variant

;; TODO: add implicit exist for defn-e

(declare noattack-o)

(defn-e nqueens-o [l]
  ([()])
  ([[[?x ?y] . ?others]]
     (nqueens-o ?others)
     (member-o ?y [1 2 3 4 5 6 7 8])
     (noattack-o [?x ?y] ?others)))

(defn-e noattack-o [q others]
  ([_ ()])
  ([[?x ?y] [[?x1 ?y1] . ?others]]
     (!= ?y ?y1)
     (nonrel/project [?y ?y1 ?x ?x1]
                     (!= (- ?y1 ?y) (- ?x1 ?x))
                     (!= (- ?y1 ?y) (- ?x ?x1)))
     (noattack-o [?x ?y] ?others)))

(defn solve-nqueens []
  (run* [q]
        (exist [y1 y2 y3 y4 y5 y6 y7 y8]
               (== q [[1 y1] [2 y2] [3 y3] [4 y4] [5 y5] [6 y6] [7 y7] [8 y8]])
               (nqueens-o q))))

(comment
  (take 1 (solve-nqueens))

  ;; 92 solutions
  (count (solve-nqueens))

  ;; 3.5s
  ;; about 10X slower that SWI
  (binding [*occurs-check* false]
   (dotimes [_ 5]
     (time
      (dotimes [_ 100]
        (doall
         (take 1 (solve-nqueens)))))))
  )

;; Bratko pg 344, finite domain, can we get close to this? there is a LOT more
;; thinking to do. do we really want to go down this path?  
