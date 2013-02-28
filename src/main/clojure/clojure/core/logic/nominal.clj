(ns clojure.core.logic.nominal
  (:refer-clojure :exclude [== hash])
  (:use [clojure.core.logic.protocols]
        [clojure.core.logic :exclude [fresh] :as l])
  (:import [java.io Writer]
           [clojure.core.logic LVar LCons]
           [clojure.core.logic.protocols IBindable ITreeTerm]))

;; =============================================================================
;; Nominal unification with fresh, hash and tie.
;;
;; Some references / inspiration:
;; alphaKanren - http://www.cs.indiana.edu/~webyrd/alphamk/alphamk.pdf
;; Nominal Unification - http://www.cl.cam.ac.uk/~amp12/papers/nomu/nomu-jv.pdf
;; http://code.google.com/p/iucs-relational-research/source/browse/trunk/lib/minikanren/nominal.sls

;; =============================================================================
;; Nominal unification protocols

(defprotocol INomSwap
  (swap-noms [t swap s]))

(defn nom-swap [a swap]
  (cond
    (= a (first swap)) (second swap)
    (= a (second swap)) (first swap)
    :else a))

(declare suspc)

(extend-protocol INomSwap
  nil
  (swap-noms [t swap s] [t s])

  Object
  (swap-noms [t swap s] [t s])

  LVar
  (swap-noms [t swap s]
    (let [t (walk s t)]
      (if (lvar? t)
        (let [v (with-meta (lvar) (meta t))
              rt (root-val s t)
              s (-> (if (subst-val? rt) (ext-no-check s v rt) s)
                  (entangle t v)  
                  (update-dom v ::nom (fnil (fn [d] (conj d t)) #{}))
                  (update-dom t ::nom (fnil (fn [d] (conj d v)) #{}))
                  ((suspc v t swap)))]
          [v s])
        (swap-noms t swap s))))

  LCons
  (swap-noms [t swap s]
    (let [[tfirst s] (swap-noms (lfirst t) swap s)
          [tnext s] (swap-noms (lnext t) swap s)]
      [(with-meta (lcons tfirst tnext) (meta t))
        s]))

  clojure.lang.IPersistentCollection
  (swap-noms [t swap s]
    (if (seq t)
      (let [[tfirst s] (swap-noms (first t) swap s)
            [tnext s] (swap-noms (next t) swap s)]
        [(with-meta (cons tfirst tnext) (meta t))
          s])
      [t s])))

(extend-protocol IMergeDomains
  clojure.lang.IPersistentSet
  (-merge-doms [a b]
    (clojure.set/union a b)))

;; =============================================================================
;; Nom

(declare nom)

(deftype Nom [lvar]
  IBindable

  Object
  (toString [_]
    (str "<nom:" (:name lvar) ">"))
  (hashCode [_]
    (.hashCode lvar))
  (equals [this o]
    (and (.. this getClass (isInstance o))
         (= lvar (:lvar o))))

  clojure.lang.IObj
  (withMeta [this new-meta]
    (nom (with-meta lvar new-meta)))
  (meta [this]
    (meta lvar))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [_ k not-found]
    (case k
      :lvar lvar
      :name (:name lvar)
      :oname (:oname lvar)
      not-found))

  IReifyTerm
  (reify-term [v s]
    (ext s v (symbol (str (if (-> s meta (:reify-noms true)) "a" (:oname v)) "_" (count s)))))

  INomSwap
  (swap-noms [t swap s]
    [(with-meta (nom-swap t swap) (meta t))
      s]))

(defn nom [lvar]
  (Nom. lvar))

(defn nom? [x]
  (instance? clojure.core.logic.nominal.Nom x))

(defn- nom-bind [sym]
  ((juxt identity
         (fn [s] `(nom (lvar '~s)))) sym))

(defn- nom-binds [syms]
  (mapcat nom-bind syms))

(defmacro fresh
  "Creates fresh noms. Goals occuring within form a logical conjunction."
  [[& noms] & goals]
  `(fn [a#]
     (-inc
      (let [~@(nom-binds noms)]
        (bind* a# ~@goals)))))

(defmethod print-method Nom [x ^Writer writer]
  (.write writer (str "<nom:" (:name x) ">")))

;; =============================================================================
;; hash: ensure a nom is free in a term

(declare tie? hash)

(defn- -hash
  [a x]
  (reify
    Object
    (toString [_]
      (str a "#" x))
    clojure.lang.IFn
    (invoke [c s]
      (let [a (walk s a)
            x (walk s x)]
        (if (lvar? a)
          (when (and
                  (not (and (lvar? x) (= x a)))
                  (tree-term? x) (not (tie? x)))
            ((composeg*
              (remcg c)
              (constrain-tree x
                (fn [t s] ((hash a t) s)))) s))
          (when (nom? a)
            (cond
              (and (tie? x)  (= (:binding-nom x) a))
              ((remcg c) s)
              (tree-term? x)
              ((composeg* 
                (remcg c)
                (constrain-tree x
                  (fn [t s] ((hash a t) s)))) s)
              (= x a)
              nil
              :else
              ((remcg c) s))))))
    IConstraintOp
    (rator [_] `hash)
    (rands [_] [a x])
    IReifiableConstraint
    (reifyc [_ v r s]
      (let [x (walk* r (walk* s x))
            a (walk* r (walk* s a))]
        ;; Filter constraints unrelated to reified variables.
        (when (and (symbol? a) (empty? (->> (list x) flatten (filter lvar?))))
          (symbol (str a "#" x)))))
    IRunnable
    (runnable? [_ s]
      (let [a (walk s a)
            x (walk s x)]
        (if (lvar? a)
          (or
           (and (lvar? x) (= x a))
           (and (tree-term? x) (not (tie? x))))
          (or
           (not (nom? a))
           (not (lvar? x))))))
    IConstraintWatchedStores
    (watched-stores [this] #{::l/subst})))

(defn hash [a t]
  (cgoal (-hash a t)))

;; =============================================================================
;; Suspensions as constraints

(defn- -do-suspc [t1 t2 swap a]
  (when (loop [vs #{t2} seen #{}]
          (let [vs (clojure.set/difference vs seen)]
            (cond
              (empty? vs)
              true
              (some #(occurs-check a % t1) vs)
              false
              :else
              (let [vs2 (apply clojure.set/union
                          (map (fn [x] (if (nil? x) #{} x))
                            (map #(get-dom a % ::nom) vs)))
                    seen (clojure.set/union vs seen)]
                (recur vs2 seen)))))
    (let [[t1 a] (swap-noms t1 swap a)]
      ((== t1 t2) a))))

(defn -suspc
  [v1 v2 swap]
  (reify
    Object
    (toString [_]
      (str "suspc" v1 v2 swap))
    clojure.lang.IFn
    (invoke [c a]
      ((composeg*
        (remcg c)
        (fn [a]
          (let [t1 (walk a v1)
                t2 (walk a v2)]
            (cond
              (not (lvar? t1))
              (-do-suspc t1 t2 swap a)
              (not (lvar? t2))
              (-do-suspc t2 t1 swap a)
              (= t1 t2)
              (loop [a* swap
                     a a]
                (if (empty? a*) a
                    (recur (rest a*) ((hash (first a*) t2) a))))
              :else ((addcg c) a))))) a))
    IConstraintOp
    (rator [_] `suspc)
    (rands [_] [v1 v2])
    IReifiableConstraint
    (reifyc [c v r a]
      (let [t1 (walk* r (walk* a v1))
            t2 (walk* r (walk* a v2))
            swap (walk* r swap)]
        (when (and
                (not (lvar? t1))
                (not (lvar? t2))
                (symbol? (first swap))
                (symbol? (second swap)))
          `(~'swap ~swap ~t1 ~t2))))
    IRunnable
    (runnable? [_ a]
      (let [t1 (walk a v1)
            t2 (walk a v2)]
        (or (not (lvar? t1)) (not (lvar? t2)) (= t1 t2))))
    IConstraintWatchedStores
    (watched-stores [this] #{::l/subst})))

(defn suspc [v1 v2 swap]
  (cgoal (-suspc v1 v2 swap)))

;; =============================================================================
;; tie: bind a nom in a term

(declare tie)

(defrecord Tie [binding-nom body]
  ITreeTerm

  IUnifyTerms
  (unify-terms [v u s]
    (cond
      (tie? u)
      (if (= (:binding-nom v) (:binding-nom u))
        (unify s (:body v) (:body u))
        (let [[t s] (swap-noms (:body v) [(:binding-nom v) (:binding-nom u)] s)]
          ((composeg* 
            (hash (:binding-nom u) (:body v))
            (== t (:body u))) s)))
      :else nil))

  IReifyTerm
  (reify-term [v s]
    (let [s (-reify* s binding-nom)]
      (let [s (-reify* s body)]
        s)))

  IWalkTerm
  (walk-term [v f]
    (with-meta
      (tie (walk-term (:binding-nom v) f)
           (walk-term (:body v) f))
      (meta v)))

  IOccursCheckTerm
  (occurs-check-term [v x s]
    (occurs-check s x (:body v)))

  IConstrainTree
  (-constrain-tree [t fc s]
    (fc (:body t) s))

  IForceAnswerTerm
  (-force-ans [v x]
    (force-ans (:body v)))

  INomSwap
  (swap-noms [t swap s]
    (let [[tbody s] (swap-noms (:body t) swap s)]
      [(with-meta (tie (nom-swap (:binding-nom t) swap) tbody) (meta t))
        s])))

(defn tie [binding-nom body]
  (Tie. binding-nom body))

(defn tie? [x]
  (instance? clojure.core.logic.nominal.Tie x))

(defmethod print-method Tie [x ^Writer writer]
  (.write writer (str " [" (:binding-nom x) "] "))
  (print-method (:body x) writer))
