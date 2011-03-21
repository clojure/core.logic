(ns logos.bench
  (:refer-clojure :exclude [reify inc ==])
  (:use [logos minikanren match]
        [logos.logic :only [first-o member-o]])
  (:require [clojure.contrib.macro-utils :as macro]))

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
  ;; ~5s
  (let [data (into [] (range 30))]
   (dotimes [_ 5]
     (time
      (dotimes [_ 1e3]
        (doall (run 1 [q] (nrev-o data q)))))))

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
  ;; SWI-Prolog 8.5s
  ;; < 4s (make-s ?)
  (binding [*occurs-check* false]
   (dotimes [_ 5]
    (time
     (dotimes [_ 1e3]
       (doall (run 1 [q] (zebra-o q)))))))
  )
  
