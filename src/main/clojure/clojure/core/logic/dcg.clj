(ns clojure.core.logic.dcg
  (:refer-clojure :exclude [reify == inc])
  (:use [clojure.core.logic minikanren prelude]))

;; TODO: think about indexing
;; TODO: note that rest args are problematic since we add two invisible args
;; TODO: support !dcg, filter !dcg clauses to generate env, as we map over
;; the clauses, if clause is not dcg clause, skip and remove the front

(defn lsym [n]
  (gensym (str "l" n "_")))

(defn !dcg? [clause]
  (and (sequential? clause)
       (let [f (first clause)]
         (and (symbol? f)
              (= (name f) "!dcg")))))

(defn ->lcons
  ([env [m :as c] i] (->lcons env c i false))
  ([env [m :as c] i quoted]
     (let [m (if quoted `(quote ~m) m)]
       `(== ~(env (dec i)) (lcons ~m ~(env i))))))

(defn mark-clauses
  ([clauses] (mark-clauses clauses 0))
  ([clauses start]
     (let [i (atom start)]
       (map (fn [clause]
              (if (!dcg? clause)
                clause
                (with-meta clause
                  {:index (swap! i clojure.core/inc)})))
            clauses))))

(defn handle-clause [env c]
  (cond
   (!dcg? c) (second c)
   (vector? c) (->lcons env c (-> c meta :index))
   (and (seq? c)
        (= (first c) `quote)
        (vector? (second c))) (->lcons env (second c) (-> c meta :index) true)
   :else (let [i (-> c meta :index)
               c (if (seq? c) c (list c))]
           (concat c [(env (dec i)) (env i)]))))

(defmacro --> [name & clauses]
  (let [r (range 1 (+ (count clauses) 2))
        lsyms (into [] (map lsym r))
        clauses (mark-clauses clauses)
        clauses (map (partial handle-clause lsyms) clauses)]
    `(defn ~name [~(first lsyms) ~(last lsyms)]
       (exist [~@(butlast (rest lsyms))]
         ~@clauses))))

(defmacro def--> [name args & clauses]
  (let [r (range 1 (+ (count clauses) 2))
        lsyms (map lsym r)
        clauses (mark-clauses clauses)
        clauses (map (partial handle-clause lsyms) clauses)]
   `(defn ~name [~@args ~(first lsyms) ~(last lsyms)]
      (exist [~@(butlast (rest lsyms))]
        ~@clauses))))

(defn handle-cclause [fsym osym cclause]
  (let [c (count cclause)
        r (range 2 (clojure.core/inc c))
        lsyms (conj (into [fsym] (map lsym r)) osym)
        clauses (mark-clauses cclause)
        clauses (map (partial handle-clause lsyms) clauses)]
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
    ('[the])
    ('[a]))
  
  (-->e n
    ('[witch])
    ('[wizard]))

  (--> v '[curses])

  (--> np det n)
  (--> vp v np)
  (--> s np vp)

  ;; we can stop the dcg transform
  (--> s np (!dcg (== 1 1)) vp)

  ;; success
  (run* [q]
    (np '[the witch] []))

  ;; success
  (run* [q]
    (s '[a witch curses the wizard] []))

  (def-->e verb [v]
    ([[:v 'eats]] '[eats]))

  (def-->e noun [n]
    ([[:n 'bat]] '[bat])
    ([[:n 'cat]] '[cat]))

  (def-->e det [d]
    ([[:d 'the]] '[the])
    ([[:d 'a]] '[a]))

  (def-->e noun-phrase [n]
    ([[:np ?d ?n]] (det ?d) (noun ?n)))
  
  (def-->e verb-phrase [n]
    ([[:vp ?v ?np]] (verb ?v) (noun-phrase ?np)))

  (def-->e sentence [s]
    ([[:s ?np ?vp]] (noun-phrase ?np) (verb-phrase ?vp)))

  (run 1 [parse-tree]
    (sentence parse-tree '[the bat eats a cat] []))

  ;; ([:s [:np [:d the] [:n bat]] [:vp [:v eats] [:np [:d a] [:n cat]]]])

  ;; ~90-100ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (run 1 [parse-tree]
         (sentence parse-tree '[the bat eats a cat] [])))))
  )
