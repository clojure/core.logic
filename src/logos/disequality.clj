(ns logos.disequality
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren
        logos.match)
  (:import [logos.minikanren Substitutions Pair]))

;; =============================================================================
;; Utilities

;; TODO: change to lazy-seq
(defn prefix [s <s]
  (if (= s <s)
    ()
    (cons (first s) (prefix (rest s) <s))))

;; =============================================================================
;; Verification

(defn ^Substitutions constraint-verify [s u v l verify cs]
  (let [uc (constraints u)]
    (if (contains? uc v)
      nil
      (let [u (remove-constraints u)
            v (if (lvar? v) (add-constraints v uc) v)]
        (make-s (-> s (dissoc u) (assoc u v)) (cons (pair u v) l) verify cs)))))

(defprotocol IDisequality
  (!=-verify [this sp]))

(declare make-store)
(declare merge-constraint)
(declare make-c)

(defn unify* [^Substitutions s m]
  (loop [[[u v :as p] & r] (seq m) result {}]
    (let [^Substitutions s' (unify s u v)]
      (cond
       (nil? p) result
       (not s') nil
       (identical? s s') (recur r result)
       :else (recur r
                    (conj result
                          (first (prefix (.l s') (.l s)))))))))

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
                                          (.l this) constraint-verify (.cs this))))))
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
(deftype Constraint [^String name m okeys hash]
  Object
  (equals [this o]
          (and (instance? Constraint o)
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

(defmethod print-method Constraint [x writer]
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
                           c (dissoc (get cmap name) u)
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
                                             (conj (or simple #{})
                                                   (first c))))
                         (let [cmap (assoc cmap name c)]
                           (ConstraintStore. vmap cmap simple)))))

  (discard-constraint [this c]
                      (let [^Constraint c c
                            name (.name c)
                            c (get cmap name)
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
                     this
                     (let [v' (walk s (get c u))]
                       (cond
                        (= v v') (recur cr (refine-constraint this u c))
                        (or (lvar? v')
                            (lvar? v)) (recur cr this)
                        :else (recur cr (discard-constraint this c)))))))
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

(comment
  ;; with pairs
  (let [[x y z a] (map lvar '(x y z a))
        s (-> empty-s
              (unify x 1)
              (unify y z))]
    (unify* s [(pair x 1) (pair y z)]))
  
  ;; was violated
  (let [[x y z a] (map lvar '(x y z a))
        s (-> empty-s
              (unify x 1)
              (unify y z))]
    (unify* s {x 1 y z}))
  
  ;; might be violated
  (let [[x y z a] (map lvar '(x y z a))
        s (-> empty-s
              (unify x 1)
              (unify y z))]
    (unify* s {x 1 y a}))

  ;; with pairs
  (let [[x y z a] (map lvar '(x y z a))
        s (-> empty-s
              (unify x 1)
              (unify y z))]
    (unify* s [(pair x 1) (pair y a)]))

  ;; 200ms
  (let [[x y z a] (map lvar '(x y z a))
        s (-> empty-s
              (unify x 1)
              (unify y z))
        p1 (pair x 1)
        p2 (pair y 2)
        c [p1 p2]]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e5]
         (unify* s c)))))

  (let [[x y z a] (map lvar '(x y z a))
        s (-> empty-s
              (unify x 1)
              (unify y z))
        c {x 1 y 2}]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e5]
         (unify* s c)))))

  ;; nil can't be violated
  (let [[x y z a] (map lvar '(x y z a))
        s (-> empty-s
              (unify x 1)
              (unify y z))]
    (unify* s {x 2 y a}))

  (let [cs (make-store)
        cs (merge-constraint cs (make-c '{x 1 y 2 z 3}))]
    [(get cs 'x) (get cs 'y)])

  (let [[x y z] (map lvar '[x y z])
        c  (make-c {x 1 y 2 z 3})]
    (dissoc c y))
  
  (let [[x y z] (map lvar '[x y z])
        cs (make-store)
        c  (make-c {x 1 y 2 z 3})
        cs (merge-constraint cs c)
        cs (refine-constraint cs c y)]
    [(get cs x) (get cs y)])

  (let [[x y z] (map lvar '[x y z])
        cs (make-store)
        c  (make-c {x 1 y 2 z 3})
        cs (merge-constraint cs c)
        cs (refine-constraint cs c y)
        cs (refine-constraint cs c z)]
    [(get cs x) (.simple cs)])
  
  (let [[x y z] (map lvar '[x y z])
        cs (make-store)
        c  (make-c {x 1 y 2 z 3})
        cs (merge-constraint cs c)
        cs (discard-constraint cs c)]
    (.vmap cs))

  (let [cs (make-store)
        cs (assoc cs 'x (make-c '{x 1}))]
    (get cs 'x))

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

  ;; 1.1s, we'll need to track how this affects
  ;; actual logic programs, constraints allow us
  ;; to fail many dead ends quickly.
  (let [[x y z] (map lvar '[x y z])
        cs (make-store)
        c  (make-c {x 1 y 2 z 3})
        cs (merge-constraint cs c)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e5]
         (-> cs
             (refine-constraint c z)
             (refine-constraint c y))))))

  ;; 500ms
  ;; linear in the number of vars, 5 takes ~850ms
  (let [[x y z a b] (map lvar '[x y z a b])
        cs (make-store)
        c  (make-c {x 1 y 2 z 3})
        cs (merge-constraint cs c)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e5]
         (discard-constraint cs c)))))

  

  ;; 350ms
  (let [[x y z] (map lvar '[x y z])
        cs (make-store)
        c  (make-c {x 1 y 2 z 3})
        cs (merge-constraint cs c)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e5]
         (refine-constraint cs c y)))))

  ;; 150ms
  (let [[x y z] (map lvar '[x y z])
        cs (make-store)
        c  (make-c {x 1 y 2 z 3})]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e5]
         (merge-constraint cs c)))))

  ;; a little bit faster
  (let [[x y z] (map lvar '[x y z])
        cs (make-store)
        c  (make-c {x 1 y 2 z 3})]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e5]
         (-> cs
             (assoc x c)
             (assoc y c)
             (assoc z c))))))

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

