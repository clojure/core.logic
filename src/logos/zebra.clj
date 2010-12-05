(ns logos.zebra
  (:refer-clojure :exclude [reify inc == take])
  (:use logos.minikanren
        logos.logic)
  (:require [clojure.contrib.macro-utils :as macro]))

(defn on-right-o [x y l]
  (cond-e
   ((exist [fst snd r]
           (== (llist fst snd r) l)
           (== fst x) (== snd y)))
   ((exist [r]
           (rest-o l r) (on-right-o x y r)))))

(defn next-to-o [x y l]
  (cond-e
   ((on-right-o x y l))
   ((on-right-o y x l))))

(defn zebra [hs]
  (macro/symbol-macrolet [_ (lvar)]
   (all
    (== [_ _ [_ _ 'milk _ _] _ _] hs)                         ;; there are five houses, the person that lives in the middle drinks milk
    (first-o hs ['norwegian _ _ _ _])                         ;; the norwegian lives in the first house
    (next-to-o ['norwegian _ _ _ _] [_ _ _ _ 'blue] hs)       ;; the norwegian lives next to the blue horse
    (on-right-o [_ _ _ _ 'ivory] [_ _ _ _ 'green] hs)         ;; green house is right of the ivory house
    (member-o ['englishman _ _ _ 'red] hs)                    ;; the englishman lives in the red house
    (member-o [_ 'kools _ _ 'yellow] hs)                      ;; kools are smoked in the yellow house
    (member-o ['spaniard _ _ 'dog _] hs)                      ;; the spaniard has a dog
    (member-o [_ _ 'coffee _ 'green] hs)                      ;; coffee is drunk in the green house
    (member-o ['ukrainian _ 'tea _ _] hs)                     ;; the ukrainian drinks tea
    (member-o [_ 'lucky-strikes 'oj _ _] hs)                  ;; the lucky strikes smoker drinks oj
    (member-o ['japanese 'parliaments _ _ _] hs)              ;; the japanese man smokes parliaments
    (member-o [_ 'oldgolds _ 'snails _] hs)                   ;; old gold smoker owns snails
    (next-to-o [_ _ _ 'horse _] [_ 'kools _ _ _] hs)          ;; kools are smoked in the house next to the horse
    (next-to-o [_ _ _ 'fox _] [_ 'chesterfields _ _ _] hs)))) ;; the man who smokes chesterfields is next to the the man who owns a fox

(comment
  (run* [q]
        (zebra q))

  ;; 22ms, kinda slow
  (dotimes [_ 10]
    (time
     (dotimes [_ 1]
       (run* [q]
             (zebra q)))))

  (macro/symbol-macrolet [_ (lvar)]
    (run* [q]
          (exist [x]
                 (== x [[_ 2] _])
                 (== q [[1 _] _])
                 (== q [_ _])
                 (== q x))))

  (macro/symbol-macrolet [_ (lvar)]
   (run* [q]
         (exist [x]
                (== x [1 _])
                (first-o `[[~_ 2] 2 3 4] x)
                (== x q))))

  ;; succeeds twice
  (run* [q]
        (on-right-o 'cat 'dog '[cat dog cat dog]))

  ;; succeed once
  (run* [q]
        (on-right-o 'dog 'cat '[cat dog cat dog]))

  (run* [q]
        (exist [x]
               (== x [_ _])
               (== q [1 _])
               (== x q)))
  )