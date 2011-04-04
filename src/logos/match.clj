(ns logos.match
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren)
  (:require [logos.logic :as logic]
            [logos.tabled :as tabled]
            [clojure.set :as set])
  (:import [logos.minikanren Substitutions]))

(declare p->term)

(defn lcons-p? [p]
  (and (coll? p)
       (not (nil? (some '#{.} p)))))

(defn p->llist [p]
  `(llist
    ~@(map p->term
           (remove #(contains? '#{.} %) p))))

(defn p->term [p]
  (cond
   (= p '_) `(lvar)
   (lcons-p? p) (p->llist p)
   (coll? p) `[~@(map p->term p)]
   :else p))

(defn lvar-sym? [s]
  (= (first (str s)) \?))

(defn extract-vars
  ([p]
     (set
      (cond
       (lvar-sym? p) [p]
       (coll? p) (filter lvar-sym? (flatten p))
       :else nil)))
  ([p seen]
     (set/difference (extract-vars p) (set seen))))

(defn ex
  ([vs t a]
     `(exist [~@vs]
             (== ~t ~a)))
  ([vs t a expr]
     `(exist [~@vs]
             (== ~t ~a)
             ~expr)))

(defn ex* [[[p a :as pa] & par] expr seen]
  (let [t    (p->term p)
        vs   (extract-vars p seen)
        seen (reduce conj seen vs)]
    (cond
     (nil? pa) expr
     (= p '_) (ex* par expr seen)
     (empty? par) (if expr
                    (ex vs t a expr)
                    (ex vs t a))
     :else (let [r (ex* par expr seen)]
             (if r
               (ex vs t a r)
               (ex vs t a))))))

(defn handle-clause [as]
  (fn [[p & expr]]
    (let [pas (partition 2 (interleave p as))]
      (ex* pas (first expr) #{}))))

(defn handle-clauses [t as cs]
  `(~t
    ~@(map list (map (handle-clause as) cs))))

(defn defn-m [t n as & cs]
  (if-let [tabled? (-> n meta :tabled)]
    `(def ~n
       (logos.tabled/tabled [~@as]
        ~(handle-clauses t as cs)))
    `(defn ~n [~@as]
       ~(handle-clauses t as cs))))

(defmacro defn-e [& rest]
  (apply defn-m `cond-e rest))

(defmacro match-e [xs & cs]
  (handle-clauses `cond-e xs cs))

;; -----------------------------------------------------------------------------
;; quick tests

(comment
  (defn-e append-o [x y z]
    ([() _ y])
    ([[?a . ?d] _ [?a . ?r]] (append-o ?d y ?r)))

  (defn-e ^:tabled append-o-tabled [x y z]
    ([() _ y])
    ([[?a . ?d] _ [?a . ?r]] (append-o ?d y ?r)))

  (run 1 [q] (append-o [1 2] [3 4] q))

  ;; 1.5s
  (dotimes [_ 5]
    (time
     (dotimes [_ 1e5]
       (doall (run 1 [q] (append-o [1 2] [3 4] q))))))

  ;; 1.6s actually a bit slower for this relation
  (dotimes [_ 5]
    (time
     (dotimes [_ 1e5]
       (doall (run 1 [q] (append-o-tabled [1 2] [3 4] q))))))

  (defn-e arc-o [x y]
    ([:a :b])
    ([:b :a])
    ([:b :d]))

  (defn-e ^:tabled path-o [x y]
    ([x y] (cond-e
            ((arc-o x y))
            ((exist [z]
               (arc-o x z)
               (path-o z y))))))

  ;; (:b :a :d)
  (run* [q] (path-o :a q))

  (defn-e test-o [x y]
    ([() _]))

  (defn-e test-2-o [x y]
    ([[_ _ ?a] _]))

  (defn test-o [x y]
    (match-e [x y]
       ([() _])
       ([[?a . ?b] [?c ?d]] (test-o ?a ?d))))
  )