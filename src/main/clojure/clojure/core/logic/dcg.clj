(ns clojure.core.logic.dcg
  (:refer-clojure :exclude [reify == inc])
  (:use [clojure.core.logic minikanren prelude]))

(defn lsym [n]
  (gensym (str "l" n "_")))

;; TODO: don't handle syms specially like this

(defn ->lcons [env [m] i]
  (let [m (if (symbol? m) `(quote ~m) m)]
    `(== ~(env (dec i)) (lcons ~m ~(env i)))))

(defn handle-clause [env c i]
  (if (vector? c)
    (->lcons env c i)
    (let [c (if (seq? c) c (list c))]
      (concat c [(env (dec i)) (env i)]))))

(defmacro --> [name & clauses]
  (let [r (range 1 (+ (count clauses) 2))
        lsyms (into [] (map lsym r))
        clauses (map (partial handle-clause lsyms) clauses r)]
    `(defn ~name [~(first lsyms) ~(last lsyms)]
       (exist [~@(butlast (rest lsyms))]
         ~@clauses))))

(defmacro def--> [name args & clauses]
  (let [r (range 1 (+ (count clauses) 2))
        lsyms (map lsym r)
        clauses (map (partial handle-clause lsyms) clauses r)]
   `(defn ~name [~@args ~(first lsyms) ~(last lsyms)]
      (exist [~@(butlast (rest lsyms))]
        ~@clauses))))

(defn handle-cclause [fsym osym cclause]
  (let [c (count cclause)
        r (range 2 (clojure.core/inc c))
        lsyms (conj (into [fsym] (map lsym r)) osym)
        clauses (map (partial handle-clause lsyms) cclause (range 1 (+ c 2)))]
    `(exist [~@(butlast (rest lsyms))]
       ~@clauses)))

(defmacro -->e [name & cclauses]
  (let [fsym (gensym "l1_")
        osym (gensym "o")]
   `(defn ~name [~fsym ~osym]
      (conde
       ~@(map list (map (partial handle-cclause fsym osym) cclauses))))))

(defmacro def-->e [name args & pcss]
  (let [fsym (gensym "l1_")
        osym (gensym "o")]
   `(defne ~name [~@args ~fsym ~osym]
      ~@(map (fn [[p & cs]]
               (list (-> p (conj '_) (conj '_))
                     (handle-cclause fsym osym cs)))
             pcss))))

(comment
  (-->e det
    ([the])
    ([a]))
  
  (-->e n
    ([witch])
    ([wizard]))

  (--> v [curses])

  (--> np det n)
  (--> vp v np)
  (--> s np vp)

  ;; success
  (run* [q]
    (np '[the witch] []))

  ;; success
  (run* [q]
    (s '[a witch curses the wizard] []))

  (def-->e verb [v]
    ([[:v 'eats]] [eats]))

  (def-->e noun [n]
    ([[:n 'bat]] [bat])
    ([[:n 'cat]] [cat]))

  (def-->e det [d]
    ([[:d 'the]] [the])
    ([[:d 'a]] [a]))

  (def-->e noun-phrase [n]
    ([[:np ?d ?n]] (det ?d) (noun ?n)))
  
  (def-->e verb-phrase [n]
    ([[:vp ?v ?np]] (verb ?v) (noun-phrase ?np)))

  (def-->e sentence [s]
    ([[:s ?np ?vp]] (noun-phrase ?np) (verb-phrase ?vp)))

  (run* [parse-tree]
    (sentence parse-tree '[the bat eats a cat] []))

  ;; ([:s [:np [:d the] [:n bat]] [:vp [:v eats] [:np [:d a] [:n cat]]]])

  ;; ~70ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e3]
       (run* [parse-tree]
         (sentence parse-tree '[the bat eats a cat] [])))))
  )
