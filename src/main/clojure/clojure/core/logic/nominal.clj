(ns clojure.core.logic.nominal
  (:refer-clojure :exclude [== hash])
  (:use [clojure.core.logic :exclude [fresh] :as l])
  (:import [java.io Writer]))

(def ^{:dynamic true} *reify-noms* true)

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

  clojure.core.logic.LVar
  (swap-noms [t swap s]
    (let [t (walk* s t)]
      (if (lvar? t)
        (let [v (with-meta (lvar) (meta t))
              rt (root-val s t)
              s (if (subst-val? rt) (ext-no-check s v rt) s)
              s (update-dom s (root-var s v) ::nom (fnil (fn [d] (conj d t)) []))
              s (update-dom s (root-var s t) ::nom (fnil (fn [d] (conj d v)) []))
              s (bind s (suspc v t swap))]
          [v s])
        (swap-noms t swap s))))

  clojure.core.logic.LCons
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
  clojure.lang.IPersistentVector
  (-merge-doms [a b]
    (concat a b)))

;; =============================================================================
;; Nom

(declare nom)

(deftype Nom [lvar]
  clojure.core.logic.IBindable

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

  clojure.core.logic.IReifyTerm
  (reify-term [v s]
    (ext s v (symbol (str (if *reify-noms* "a" (:oname v)) "_" (count s)))))

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
  ([a x] (-hash a x nil))
  ([a x _id]
    (reify
      Object
      (toString [_]
        (str a "#" x))
      clojure.lang.IFn
      (invoke [c s]
        (let [a (walk* s a)
              x (walk* s x)]
          (if (lvar? a)
            (when (and
                    (not (and (lvar? x) (= x a)))
                    (tree-term? x) (not (tie? x)))
              (bind* s
                (remcg c)
                (constrain-tree x
                  (fn [t s] (bind s (hash a t))))))
            (when (nom? a)
              (cond
                (and (tie? x)  (= (:binding-nom x) a))
                (bind s (remcg c))
                (tree-term? x)
                (bind* s
                  (remcg c)
                  (constrain-tree x
                    (fn [t s] (bind s (hash a t)))))
                (= x a)
                nil
                :else
                (bind s (remcg c)))))))
      clojure.core.logic.IConstraintId
      (id [this] _id)
      clojure.core.logic.IWithConstraintId
      (with-id [this _id]
        (-hash a x _id))
      clojure.core.logic.IConstraintOp
       (rator [_] `hash)
       (rands [_] [a x])
      clojure.core.logic.IReifiableConstraint
      (reifyc [_ v r s]
        (let [x (walk* r (walk* s x))
              a (walk* r (walk* s a))]
          ;; Filter constraints unrelated to reified variables.
          (when (and (symbol? a) (empty? (->> (list x) flatten (filter lvar?))))
            (symbol (str a "#" x)))))
      clojure.core.logic.IRunnable
      (runnable? [_ s]
        (let [a (walk* s a)
              x (walk* s x)]
          (if (lvar? a)
            (or
              (and (lvar? x) (= x a))
              (and (tree-term? x) (not (tie? x))))
            (or
              (not (nom? a))
              (not (lvar? x))))))
      clojure.core.logic.IConstraintWatchedStores
      (watched-stores [this] #{::clojure.core.logic/subst}))))

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
              (let [vs2 (set
                          (flatten
                            (map (fn [x] (if (nil? x) [] x))
                              (map #(get-dom a (root-var a %) ::nom) vs))))
                    seen (clojure.set/union vs seen)]
                (recur vs2 seen)))))
    (let [[t1 a] (swap-noms t1 swap a)]
      (bind a (== t1 t2)))))

(defn -suspc
  ([v1 v2 swap] (-suspc v1 v2 swap nil))
  ([v1 v2 swap _id]
    (reify
      Object
      (toString [_]
        (str "suspc" v1 v2 swap))
      clojure.lang.IFn
      (invoke [c a]
        (bind* a
          (fn [a]
            (let [t1 (walk a v1)
                  t2 (walk a v2)]
              (cond
                (not (lvar? t1))
                (-do-suspc t1 t2 swap a)
                (not (lvar? t2))
                (-do-suspc t2 t1 swap a)
                :else
                (do (assert (= t1 t2))
                  (loop [a* swap
                         a a]
                    (if (empty? a*) a
                      (recur (rest a*) (bind a (hash (first a*) t2)))))))))
          (remcg c)))
      clojure.core.logic.IConstraintId
      (id [this] _id)
      clojure.core.logic.IWithConstraintId
      (with-id [this _id]
        (-suspc v1 v2 swap _id))
      clojure.core.logic.IConstraintOp
      (rator [_] `suspc)
      (rands [_] [v1 v2])
      clojure.core.logic.IReifiableConstraint
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
      clojure.core.logic.IRunnable
      (runnable? [_ a]
        (let [t1 (walk a v1)
              t2 (walk a v2)]
          (or (not (lvar? t1)) (not (lvar? t2)) (= t1 t2))))
      clojure.core.logic.IConstraintWatchedStores
      (watched-stores [this] #{::clojure.core.logic/subst}))))

(defn suspc [v1 v2 swap]
  (cgoal (-suspc v1 v2 swap)))

;; =============================================================================
;; tie: bind a nom in a term

(declare tie)

(deftype Tie [binding-nom body _meta]
  clojure.core.logic.ITreeTerm

  Object
  (toString [_]
    (str "<tie:" binding-nom "." body ">"))
  (hashCode [_]
    (.hashCode body))
  (equals [this o]
    (and (.. this getClass (isInstance o))
         (and (= binding-nom (:binding-nom o)) (= body (:body o)))))

  clojure.lang.IObj
  (withMeta [this new-meta]
    (Tie. binding-nom body _meta))
  (meta [this]
    _meta)

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [_ k not-found]
    (case k
      :binding-nom binding-nom
      :body body
      not-found))

  clojure.core.logic.IUnifyTerms
  (unify-terms [v u s]
    (cond
      (tie? u)
      (if (= (:binding-nom v) (:binding-nom u))
        (unify s (:body v) (:body u))
        (let [[t s] (swap-noms (:body v) [(:binding-nom v) (:binding-nom u)] s)]
          (bind* s
            (hash (:binding-nom u) (:body v))
            (== t (:body u)))))
      :else nil))

  clojure.core.logic.IReifyTerm
  (reify-term [v s]
    (let [s (-reify* s binding-nom)]
      (let [s (-reify* s body)]
        s)))

  clojure.core.logic.IWalkTerm
  (walk-term [v f]
    (with-meta
      (tie (walk-term (:binding-nom v) f)
           (walk-term (:body v) f))
      (meta v)))

  clojure.core.logic.IOccursCheckTerm
  (occurs-check-term [v x s]
    (occurs-check s x (:body v)))

  clojure.core.logic.IConstrainTree
  (-constrain-tree [t fc s]
    (fc (:body t) s))

  clojure.core.logic.IForceAnswerTerm
  (-force-ans [v x]
    (force-ans (:body v)))

  INomSwap
  (swap-noms [t swap s]
    (let [[tbody s] (swap-noms (:body t) swap s)]
      [(with-meta (tie (nom-swap (:binding-nom t) swap) tbody) (meta t))
        s])))

(defn tie [binding-nom body]
  (Tie. binding-nom body nil))

(defn tie? [x]
  (instance? clojure.core.logic.nominal.Tie x))

(defmethod print-method Tie [x ^Writer writer]
  (.write writer (str " [" (:binding-nom x) "] "))
  (print-method (:body x) writer))
