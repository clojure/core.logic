(ns logos.disequality
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren
        logos.match)
  (:import [logos.minikanren Substitutions Pair]))

;; =============================================================================
;; Utilities

(defn prefix [s <s]
  (if (= s <s)
    ()
    (cons (first s) (prefix (rest s) <s))))

;; =============================================================================
;; Verification

;; COMPLEX
;; we lookup u in the constraint store
;; each key points to multiple constraints
(defn ^Substitutions constraint-verify [s u v l verify cs]
  (let [uc (constraints u)]
    (if (contains? uc v)
      nil
      (let [u (remove-constraints u)
            v (if (lvar? v) (add-constraints v uc) v)]
        (make-s (-> s (dissoc u) (assoc u v)) (cons (pair u v) l) verify cs)))))

(defprotocol IDisequality
  (!=-verify [this sp]))

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
                                    u (add-constraint u v)]
                                (if (contains? s u)
                                  (let [v' (s u)]
                                    (make-s (-> s (dissoc u) (assoc u v'))
                                            (.l this) constraint-verify (.cs this)))
                                  (make-s (assoc s u unbound)
                                          (.l this) constraint-verify (.cs this))))))))))))

;; =============================================================================
;; Constraint

;; need hashCode and equals for propagation bit
(deftype Constraint [^String name m okeys]
  clojure.lang.Associative
  (containsKey [this key]
               (contains? m key))
  (entryAt [this key]
           (.entryAt m key))
  clojure.lang.IPersistentMap
  (without [this key]
           (Constraint. name (dissoc m key) okeys))
  clojure.lang.ISeq
  (seq [_] (seq m))
  (count [_] (count m))
  clojure.lang.ILookup
  (valAt [this key]
         (m key)))

;; NOTE: gensym is slow don't use it directly
(defn ^Constraint make-c [m]
  (Constraint. (str "constraint-" (. clojure.lang.RT (nextID))) m (keys m)))

(defn constraint? [x]
  (instance? Constraint x))

(defmethod print-method Constraint [x writer]
  (.write writer (str "<constraint:" (.m ^Constraint x) ">")))

;; =============================================================================
;; Constraint Store

(defprotocol IConstraintStore
  (merge-constraint [this c])
  (refine-constraint [this u c])
  (discard-constraint [this c])
  (propagate [this s u v])
  (get-simplified [this])
  (discard-simplified [this]))

(deftype ConstraintStore [vmap cmap simple]
  IConstraintStore
  (merge-constraint [this c]
                    (let [^Constraint c c
                          ks (keys (.m c))]
                      (reduce (fn [cs k] (assoc cs k c)) this ks)))

  (refine-constraint [this u c]
                     (let [^Constraint c c
                           c (dissoc c u)]
                       (let [name (.name c)
                             vmap (update-in vmap [u] #(disj % name))]
                        (if (= (count c) 1)
                          (let [cmap (dissoc cmap c)]
                           (ConstraintStore. vmap cmap
                                             (conj (or simple #{})
                                                   (first (.m c)))))
                          (let [cmap (assoc-in cmap [name] cmap)]
                            (ConstraintStore. vmap cmap simple))))))

  (discard-constraint [this c]
                      (let [^Constraint c c
                            name (.name c)
                            okeys (.okeys c)
                            cmap (dissoc cmap name)
                            vmap (reduce (fn [m k]
                                           (update-in m [k] #(disj % name)))
                                         okeys)]
                        (ConstraintStore. vmap cmap simple)))

  (propagate [this s u v]
             (when (contains? vmap u)
               (let [cs (get this u)]
                 (loop [[c & cr] cs me this]
                   (if (nil? c)
                     this
                     (let [[u' v'] (find c u)
                           v' (walk s v')] ;; u' should be fully walked, but maybe not v'
                       (cond
                        (= v v') (recur cr (refine-constraint this u c))
                        (lvar? v') (recur cr this)
                        :else (recur cr (discard-constraint this c)))))))))

  (get-simplified [this] simple)

  (discard-simplified [this] (ConstraintStore. vmap cmap nil))

  clojure.lang.Associative
  (assoc [this k v]
    (if (constraint? v)
      (let [name (.name ^Constraint v)]
        (ConstraintStore. (update-in vmap [k] (fnil #(conj % name) #{}))
                          (assoc cmap name v)
                          nil))
      (throw (Exception. "Adding something which is not a constraint"))))

  (containsKey [this key]
               (contains? vmap key))

  (entryAt [this key]
           (when (contains? vmap key)
             (let [val (vec (map #(cmap %) (vmap key)))]
               (clojure.core/reify
                clojure.lang.IMapEntry
                (key [_] key)
                (val [_] val)
                Object
                (toString [_] (.toString [key val]))))))

  clojure.lang.ILookup
  (valAt [this key]
         (when (contains? vmap key)
           (vec (map #(cmap %) (vmap key))))))

(defn ^ConstraintStore make-store []
  (ConstraintStore. {} {} nil))

;; =============================================================================
;; Syntax

(defmacro != [u v]
  `(fn [a#]
     (!=-verify a# (unify a# ~u ~v))))


(comment
  (let [[x y z] (map lvar '[x y z])
        cs (-> (make-store)
               (assoc x (make-c {1 2}))
               (assoc x (make-c {2 3})))]
    (get cs x))

  (let [[x y z] (map lvar '[x y z])
        cs (-> (make-store)
               (assoc x (make-c {1 2}))
               (assoc x (make-c {2 3})))]
    (.entryAt cs x))

  (let [[x y z] (map lvar '[x y z])
        cs (-> (make-store)
               (merge-constraint (make-c {x 1 y 2})))]
    (get cs x))

  ;; 150ms
  (let [[x y z] (map lvar '[x y z])
        cs (make-store)
        c  (make-c {x 1 y 2})]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e5]
         (merge-constraint cs c)))))

  ;; nice ~300ms, little overhead for simplified constraints
  (dotimes [_ 10]
    (let [[x y z] (map lvar '[x y z])]
     (time
      (dotimes [_ 1e6]
        (-> ((== x 1) empty-s)
            ((!= x 1)))))))

  (run* [q]
        (exist [x]
               (== x 1)
               (!= x 1)
               (== q x)))

  (run* [q]
        (exist [x]
               (== x 1)
               (!= x 2)
               (== q x)))

  (run* [q]
        (exist [x]
               (!= x 2)
               (== x 1)
               (== q x)))

  (run* [q]
        (exist [x]
               (!= x 1)
               (== x 1)
               (== q x)))

  (run* [q]
        (exist [x y]
               (== x 1)
               (!= x y)
               (== q x)))

  (run* [q]
        (exist [x y]
               (== x y)
               (!= x y)
               (== q x)))
  
  ;; NOTE: tri subst preserve never setting a var twice

  ;; should fail
  (let [[x y] (map lvar '[x y z])
        x (add-constraint x 1)]
    (constraint-verify {x y} x 1 (cons (pair x y) nil) constraint-verify nil))

  ;; should move simple constraints
  ;; #{1}
  (let [[x y z] (map lvar '[x y z])
        y (add-constraint y 1)
        ns (constraint-verify {x y} y z (cons (pair x y) nil) constraint-verify nil)]
    (constraints ((.s ns) y)))

  ;; nil
  (let [[x y z] (map lvar '[x y z])
        y (add-constraint y 1)
        ns (constraint-verify {x y} y 2 (cons (pair x y) nil) constraint-verify nil)]
    (constraints (first (keys (.s ns)))))

  ;; #{1}
  (let [m {}
        y (lvar 'y)
        yc (add-constraint y 1)]
    (-> m
        (assoc y 2)
        (dissoc y)
        (assoc yc 3)
        keys
        first
        constraints))

  ;; this works
  ;; #{1}
  (let [y (lvar 'x)
        yc (add-constraint y 1)
        s {yc 1}]
    (constraints (first (find s y))))
 )

