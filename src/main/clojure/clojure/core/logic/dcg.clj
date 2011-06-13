(ns clojure.core.logic.dcg
  (:refer-clojure :exclude [reify == inc])
  (:use [clojure.core.logic minikanren prelude]))

(defn lsym [n]
  (gensym (str "l" n "_")))

;; TODO: don't handle syms specially like this

(defn ->lcons [env [m] i]
  (let [m (if (symbol? m) `(quote ~m) m)]
    `(== (lcons ~m ~(env i)) ~(env (dec i)))))

(defn handle-clause [env c i]
  (if (vector? c)
    (->lcons c i)
    (let [c (if (seq? c) c (list c))]
      (concat c [(env (dec i)) (env i)]))))

(defmacro --> [name & clauses]
  (let [c (count clauses)
        r (range 1 (+ c 2))
        lsyms (into [] (map lsym r))
        f (println lsyms)
        clauses (map (partial handle-clause lsyms) clauses r)]
    `(defn ~name [~(first lsyms) ~(last lsyms)]
       (exist [~@(butlast (rest lsyms))]
         ~@clauses))))

(defn handle-cclauses [fsym osym cclause]
  (let [c (count cclause)
        r (range 1 (clojure.core/inc c))
        lsyms (conj (into [fsym] (map lsym r)) osym)
        clauses (map (partial handle-clause lsym) cclause)]
    `(exist [~@lsyms]
       ~@clauses)))

(defmacro -->e [name & cclauses]
  (let [fsym (gensym "l1_")
        osym (gensym "o")]
   `(defne ~name [~fsym ~osym]
      ~@(map (partial handle-cclauses fsym osym) cclauses))))

(defmacro def--> [name args & clauses]
  (let [c (count clauses)
        r (range 1 (+ c 2))
        lsyms (map lsym r)
        clauses (map handle-clause clauses r)]
   `(defn ~name [~@args ~(first lsyms) ~(last lsyms)]
      (exist [~@(butlast (rest lsyms))]
        ~@clauses))))

(defmacro def-->e [name args & clauses])

(comment
  (--> s np vp)
  (--> s (np subject) vp)
  (--> n [go] [to])
  (-->e det
    ([the])
    ([a]))
  (-->e np-subject
    ((det) (n))
    ((pro-subject)))
  )
