(ns clojure.core.logic.disequality
  (:refer-clojure :exclude [reify == inc])
  (:use clojure.core.logic.minikanren
        clojure.core.logic.match)
  (:import [clojure.core.logic.minikanren Substitutions Pair]
           [java.io Writer]))

;; =============================================================================
;; Utilities

;; TODO: change to lazy-seq
(defn prefix [s <s]
  (if (= s <s)
    ()
    (cons (first s) (prefix (rest s) <s))))

;; =============================================================================
;; Verification

(declare make-store)
(declare merge-constraint)
(declare make-c)
(declare propagate)
(declare get-simplified)
(declare discard-simplified)
(declare constraint-verify)

(defn ^Substitutions constraint-verify-simple [^Substitutions s u v]
  (let [uc (set (map #(walk s %) (constraints u)))]
    (if (contains? uc v)
      nil
      (let [u (remove-constraints u)
            v (if (lvar? v) (add-constraints v uc) v)]
        (make-s (-> (.s s) (dissoc u) (assoc u v))
                (cons (pair u v) (.l s))
                constraint-verify
                (.cs s))))))

(defn ^Substitutions constraint-verify [^Substitutions s u v]
  (when-let [s (constraint-verify-simple s u v)]
    (if-let [cs (.cs s)]
      (let [cs (propagate cs s u v)
            ^Substitutions s (reduce (fn [s [u c]]
                                       (constrain s u c))
                                     s (get-simplified cs))
            s (make-s (.s s) (.l s)
                      constraint-verify (discard-simplified cs))]
        (constraint-verify-simple s (get-var s u) v))
      s)))

(defprotocol IDisequality
  (!=-verify [this sp]))

(defn unify* [^Substitutions s m]
  (loop [[[u v :as p] & r] (seq m) result {}]
    (let [^Substitutions sp (unify s u v)]
      (cond
       (nil? p) result
       (not sp) nil
       (identical? s sp) (recur r result)
       :else (recur r
                    (conj result
                          (first (prefix (.l sp) (.l s)))))))))

(extend-type Substitutions
  IDisequality
  (!=-verify [this sp]
             (let [^Substitutions sp sp]
               (cond
                (not sp) this
                (= this sp) nil
                :else (let [[[u v] :as c] (prefix (.l sp) (.l this))
                            simple (= (count c) 1)]
                        (if simple
                          (let [u (walk this u)
                                v (walk this v)]
                            (if (= u v)
                              nil
                              (let [s (.s this)
                                    u (add-constraint u v)
                                    s (if (contains? s u)
                                        (let [uv (s u)]
                                          (-> s (dissoc u) (assoc u uv)))
                                        (assoc s u unbound))
                                    v (if (lvar? v) (add-constraint v u) v)
                                    s (if (contains? s v)
                                        (let [vv (s v)]
                                          (-> s (dissoc v) (assoc v vv)))
                                        (assoc s v unbound))]
                                (make-s s (.l this) constraint-verify (.cs this)))))
                          (let [r (unify* this c)]
                            (cond
                             (nil? r) this
                             (empty? r) nil
                             :else (make-s (.s this) (.l this) constraint-verify
                                           (-> (or (.cs this) (make-store))
                                               (merge-constraint
                                                (make-c r))))))))))))

;; =============================================================================
;; Constraint

;; need hashCode and equals for propagation bit
(deftype Constraint [^String name ^clojure.lang.Associative m okeys hash]
  Object
  (equals [this o]
          (and (.. this getClass (isInstance o))
               (let [^Constraint o o]
                 (identical? name (.name o)))))
  (hashCode [_] hash)
  clojure.lang.Associative
  (containsKey [this key]
               (contains? m key))
  (entryAt [this key]
           (.entryAt m key))
  clojure.lang.IPersistentMap
  (without [this key]
           (Constraint. name (dissoc m key) okeys hash))
  clojure.lang.ISeq
  (first [_] (first m))
  (seq [_] (seq m))
  (count [_] (count m))
  clojure.lang.ILookup
  (valAt [this key]
         (m key)))

;; NOTE: gensym is slow don't use it directly
(defn ^Constraint make-c [m]
  (let [name (str "constraint-" (. clojure.lang.RT (nextID)))]
    (Constraint. name m (keys m) (.hashCode name))))

(defn constraint? [x]
  (instance? Constraint x))

(defmethod print-method Constraint [x ^Writer writer]
           (.write writer (str "<constraint:" (.m ^Constraint x) ">")))

;; =============================================================================
;; Constraint Store

(defprotocol IConstraintStore
  (merge-constraint [this c])
  (refine-constraint [this c u])
  (discard-constraint [this c])
  (propagate [this s u v])
  (get-simplified [this])
  (discard-simplified [this]))

(deftype ConstraintStore [vmap cmap simple]
  IConstraintStore
  (merge-constraint [this c]
                    (let [ks (keys c)]
                      (reduce (fn [cs k] (assoc cs k c)) this ks))) ;; NOTE: switch to loop/recur?

  (refine-constraint [this c u]
                     (let [^Constraint c c
                           name (.name c)
                           ^Constraint c (dissoc (get cmap name) u)
                           vmap (update-in vmap [u] #(disj % name))
                           vmap (if (empty? (vmap u))
                                  (dissoc vmap u)
                                  vmap)]
                       (if (= (count c) 1)
                         (let [okeys (.okeys c)
                               cmap (dissoc cmap name)
                               vmap (reduce (fn [m v]
                                              (update-in m [v] #(disj % name)))
                                            vmap okeys)] ;; NOTE: hmm not all these keys exist
                           ;; TODO: clear out empty vars like below
                           (ConstraintStore. vmap cmap
                                             (conj (or simple [])
                                                   (first c))))
                         (let [cmap (assoc cmap name c)]
                           (ConstraintStore. vmap cmap simple)))))

  (discard-constraint [this c]
                      (let [^Constraint c c
                            name (.name c)
                            ^Constraint c (get cmap name)
                            okeys (.okeys c)
                            cmap (dissoc cmap name)
                            vmap (reduce (fn [m v] ;; TODO: combine
                                           (update-in m [v] #(disj % name)))
                                         vmap okeys)
                            vmap (reduce (fn [m v]
                                           (if (empty? (m v))
                                             (dissoc m v)
                                             m))
                                         vmap okeys)]
                        (ConstraintStore. vmap cmap simple)))

  (propagate [this s u v]
             (if (contains? vmap u)
               (let [cs (get this u)]
                 (loop [[c & cr] cs me this]
                   (if (nil? c)
                     me
                     (let [vp (walk s (get c u))]
                       (cond
                        (= vp v) (recur cr (refine-constraint me c u))
                        (or (lvar? vp)
                            (lvar? v)) (recur cr me)
                            :else (recur cr (discard-constraint me c)))))))
               this))

  (get-simplified [this] simple)

  (discard-simplified [this] (ConstraintStore. vmap cmap nil))

  clojure.lang.Associative
  (assoc [this u c]
    (if (constraint? c)
      (let [name (.name ^Constraint c)]
        (ConstraintStore. (update-in vmap [u] (fnil #(conj % name) #{}))
                          (assoc cmap name c) simple))
      (throw (Exception. "Adding something which is not a constraint"))))

  (containsKey [this key]
               (contains? vmap key))

  (entryAt [this key]
           (when (contains? vmap key)
             (let [val (when-let [s (seq (map #(cmap %) (vmap key)))]
                         (vec s))]
               (clojure.core/reify
                clojure.lang.IMapEntry
                (key [_] key)
                (val [_] val)
                Object
                (toString [_] (.toString [key val]))))))

  clojure.lang.ILookup
  (valAt [this key]
         (when (contains? vmap key)
           (when-let [s (seq (map #(cmap %) (vmap key)))]
             (vec s)))))

(defn ^ConstraintStore make-store []
  (ConstraintStore. {} {} nil))

;; =============================================================================
;; Syntax

(defmacro != [u v]
  `(fn [a#]
     (!=-verify a# (unify a# ~u ~v))))
