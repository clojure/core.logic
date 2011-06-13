(ns clojure.core.logic.dcg
  (:refer-clojure :exclude [reify == inc])
  (:use [clojure.core.logic minikanren prelude]))


(defmacro --> [name & clauses]
  (let [c (count clauses)
        lsym (fn [n]
               (symbol (str "l" n)))
        lsyms (map lsym (range 1 (+ c 2)))
        clauses (map #(if (seq? %) % (list %)) clauses)
        clauses (map #(concat %1 [%2 %3]) clauses lsyms (rest lsyms))]
    `(defn ~name [~(first lsyms) ~(last lsyms)]
       (exist [~@(butlast (rest lsyms))]
         ~@clauses))))

(defmacro -->e [name & body]
  )

(defmacro def--> [name args & body])

(defmacro def-->e [name args & body])

(comment
  (--> s np vp)
  (--> s (np subject) vp)
  
  (--> n '[go] (label x) '[to])
  
  (-->e n '[witch] ...)

  (-->e np-subject
        ((det) (n))
        ((pro-subject)))

  (def--> foo [x y]
    )

  (defn n [l1 l2]
    (matche l1
      ()))

  ;; difference lists
  (run* [q]
    (exist [x]
      (== q (llist 1 2 3 x))
      (== x '(4 5 6))))

  ;; 864ms for 12
  ;; 980ms for 13
  ;; 1159ms for 14
  ;; cost w/o reification?
  ;; 597 ms w/o reification for 14
  (dotimes [_ 10]
    (let [x (lvar 'x)
          l (llist 1 2 3 4 5 6 7 8 9 x)
          y (lvar 'y)
          z (lvar 'z)
          a (lvar 'a)
          b (lvar 'b)]
     (time
      (dotimes [_ 1e5]
        (run* [q]
          (== q l)
          (== x (lcons 10 y))
          (== y (lcons 11 z))
          (== z (lcons 12 a))
          (== a (lcons 13 b))
          (== b '(14)))))))

  ;; 140ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (doall (concat '(1 2 3 4 6 7 8 9) '(0))))))

  ;; 400ms for 12
  ;; 560 for 13
  ;; 744 for 14
  (dotimes [_ 10]
    (let [l '(1 2 3 4 6 7 8 9)]
     (time
      (dotimes [_ 1e5]
        (doall (concat (concat (concat (concat (concat l '(10)) '(11)) '(12)) '(13)) '(14)))))))
  )
