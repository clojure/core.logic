(ns clojure.core.logic.prelude
  (:refer-clojure :exclude [reify == inc])
  (:use clojure.core.logic.minikanren)
  (:require [clojure.set :as set]
            [clojure.core.logic.nonrel :as nonrel]
            [clojure.core.logic.tabled :as tabled]
            [clojure.core.logic.match :as match]))

;; =============================================================================
;; Useful goals

(defn nilo [a]
  (== nil a))

(defn emptyo [a]
  (== '() a))

(defn conso [a d l]
  (== (lcons a d) l))

(defn firsto [l a]
  (exist [d]
    (conso a d l)))

(defn resto [l d]
  (exist [a]
    (== (lcons a d) l)))

(defn membero [x l]
  (conde
    ((firsto l x))
    ((exist [r]
       (resto l r)
       (membero x r)))))

;; =============================================================================
;; Convenient Goal Fns

(defmacro defne [& rest]
  (apply match/defnm `conde rest))

(defmacro matche [xs & cs]
  (match/handle-clauses `conde xs cs))

;; -----------------------------------------------------------------------------
;; defnu, defna, matcha, matchu

;; TODO: we need to rethink defna and defnu, the unification comes first
;; the *question* should come first

(defmacro defna [& rest]
  (apply match/defnm 'clojure.core.logic.nonrel/conda rest))

(defmacro defnu [& rest]
  (apply match/defnm 'clojure.core.logic.nonrel/condu rest))

(defmacro matcha [xs & cs]
  (match/handle-clauses 'clojure.core.logic.nonrel/conda xs cs))

(defmacro matchu [xs & cs]
  (match/handle-clauses 'clojure.core.logic.nonrel/condu xs cs))

;; =============================================================================
;; Rel

(defn to-stream [aseq]
  (when (seq aseq)
    (choice (first aseq)
            (fn [] (to-stream (next aseq))))))

(defmacro def-arity-exc-helper []
  (try
    (Class/forName "clojure.lang.ArityException")
    `(defn arity-exc-helper [~'name ~'n]
       (fn [~'& ~'args]
         (throw (clojure.lang.ArityException. ~'n (str ~'name)))))
    (catch java.lang.ClassNotFoundException e
     `(defn ~'arity-exc-helper [~'name ~'n]
        (fn [~'& ~'args]
          (throw (java.lang.IllegalArgumentException.
                  (str "Wrong number of args (" ~'n ") passed to:" ~'name))))))))

(def-arity-exc-helper)

(defn sym-helper [prefix n]
  (symbol (str prefix n)))

(def f-sym (partial sym-helper "f"))
(def a-sym (partial sym-helper "a"))

(defn ->sym [& args]
  (symbol (apply str args)))

(defn defrel-helper [name arity args]
  (let [r (range 1 (+ arity 2))
        arity-excs (fn [n] `(arity-exc-helper '~name ~n))]
    (if (seq args)
      `(do
         (def ~name (~'clojure.core.logic.prelude.Rel. '~name (atom {}) nil ~@(map arity-excs r)))
         (extend-rel ~name ~@args))
      `(def ~name (~'clojure.core.logic.prelude.Rel. '~name (atom {}) nil ~@(map arity-excs r))))))

(defmacro def-apply-to-helper [n]
  (let [r (range 1 (clojure.core/inc n))
        args (map a-sym r)
        arg-binds (fn [n]
                    (mapcat (fn [a]
                              `(~a (first ~'arglist) ~'arglist (next ~'arglist)))
                            (take n args)))
        case-clause (fn [n]
                      `(~n (let [~@(arg-binds (dec n))]
                            (.invoke ~'ifn ~@(take (dec n) args)
                                     (clojure.lang.Util/ret1 (first ~'arglist) nil)))))]
   `(defn ~'apply-to-helper [~(with-meta 'ifn {:tag clojure.lang.IFn}) ~'arglist]
      (case (clojure.lang.RT/boundedLength ~'arglist 20)
            ~@(mapcat case-clause r)))))

(def-apply-to-helper 20)

(defprotocol IRel
  (setfn [this arity f])
  (indexes-for [this arity])
  (add-indexes [this arity index]))

;; TODO: consider moving the set/indexes inside Rel, perf implications?

(defmacro RelHelper [arity]
  (let [r (range 1 (+ arity 2))
        fs (map f-sym r)
        mfs (map #(with-meta % {:volatile-mutable true :tag clojure.lang.IFn})
                 fs)
        create-sig (fn [n]
                     (let [args (map a-sym (range 1 (clojure.core/inc n)))]
                       `(~'invoke [~'_ ~@args]
                                  (~(f-sym n) ~@args))))
        set-case (fn [[f arity]]
                   `(~arity (set! ~f ~'f)))]
    `(do
       (deftype ~'Rel [~'name ~'indexes ~'meta
                       ~@mfs]
         clojure.lang.IObj
         (~'withMeta [~'_ ~'meta]
           (~'Rel. ~'name ~'indexes ~'meta ~@fs))
         (~'meta [~'_]
           ~'meta)
         clojure.lang.IFn
         ~@(map create-sig r)
         (~'applyTo [~'this ~'arglist]
            (~'apply-to-helper ~'this ~'arglist))
         ~'IRel
         (~'setfn [~'_ ~'arity ~'f]
           (case ~'arity
                 ~@(mapcat set-case (map vector fs r))))
         (~'indexes-for [~'_ ~'arity]
           ((deref ~'indexes) ~'arity))
         (~'add-indexes [~'_ ~'arity ~'index]
           (swap! ~'indexes assoc ~'arity ~'index)))
       (defmacro ~'defrel [~'name ~'& ~'rest]
         (defrel-helper ~'name ~arity ~'rest)))))

(RelHelper 20)

(defn index-sym [name arity o]
  (->sym name "_" arity "-" o "-index"))

(defn set-sym [name arity]
  (->sym name "_" arity "-set"))

;; TODO: for arity greater than 20, we need to use rest args

(defmacro extend-rel [name & args]
  (let [arity (count args)
        r (range 1 (clojure.core/inc arity))
        as (map a-sym r)
        indexed (vec (filter (fn [[a i]]
                               (-> a meta :index))
                             (map vector
                                  args
                                  (range 1 (clojure.core/inc arity)))))
        check-lvar (fn [[o i]]
                     (let [a (a-sym i)]
                       `((not (~'lvar? (~'walk ~'a ~a)))
                         ((deref ~(index-sym name arity o)) (~'walk ~'a ~a)))))
        indexed-set (fn [[o i]]
                      `(def ~(index-sym name arity o) (atom {})))]
    (if (<= arity 20)
     `(do
        (def ~(set-sym name arity) (atom #{}))
        ~@(map indexed-set indexed)
        (add-indexes ~name ~arity '~indexed)
        (setfn ~name ~arity
               (fn [~@as]
                 (fn [~'a]
                   (let [set# (cond
                               ~@(mapcat check-lvar indexed)
                               :else (deref ~(set-sym name arity)))]
                     (~'to-stream
                      (->> set#
                           (map (fn [cand#]
                                  (when-let [~'a (~'unify ~'a [~@as] cand#)]
                                    ~'a)))
                           (remove nil?)))))))))))

;; TODO: Should probably happen in a transaction

(defn facts
  ([rel [f :as tuples]] (facts rel (count f) tuples))
  ([^Rel rel arity tuples]
     (let [rel-set (var-get (resolve (set-sym (.name rel) arity)))
           tuples (map vec tuples)]
       (swap! rel-set (fn [s] (into s tuples)))
       (let [indexes (indexes-for rel arity)]
         (doseq [[o i] indexes]
           (let [index (var-get (resolve (index-sym (.name rel) arity o)))]
             (let [indexed-tuples (map (fn [t]
                                         {(nth t (dec i)) #{t}})
                                       tuples)]
               (swap! index
                      (fn [i]
                        (apply merge-with set/union i indexed-tuples))))))))))

(defn fact [rel & tuple]
  (facts rel [(vec tuple)]))