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

;; SIMPLE
;; if u has no constraints just extend s
;; if u's constraints contain v, fail
;; if v is an lvar move u's constraints over to support tri subst
;; if v is not an lvar, we can discard the constraints
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

;; when loop, we should probably just operate through the store, not reaching
;; inside, think about it tomorrow

;; seems the simplest approach otherwise a lot of bookkeeping in the loop. We do
;; pay for ConstraintStore creation, but that's small compared to updating a
;; constraint - (removing it from vmap) (removing it from cmap).

(defprotocol IConstraintStore
  (merge-constraint [this c])
  (propagate [this s u v])
  (get-simplified [this])
  (discard-simplified [this]))

(defn unify* [^Substitutions s c]
  (loop [[[u v :as b] & cr] c nc #{}]
    (let [^Substitutions s' (unify s u v)]
      (cond
       (nil? b) (if (seq nc) nc false) ;; are we done?
       (or (identical? s s') (not s')) (recur cr nc) ;; violated sub-constraint or a discard
       :else (recur cr (conj nc (prefix (.l s') (.l s))))))))

(deftype ConstraintStore [vmap cmap simple]
  IConstraintStore
  (merge-constraint [this c]
                    (let [^Constraint c c
                          ks (keys (.m c))]
                      (reduce (fn [cs k] (assoc cs k c)) this ks)))

  (propagate [this s u v]
             (when (contains? vmap u)
               (let [cs (get this u)]
                 (loop [[c & cr] cs ncs [] simple #{}]
                   (let [[u' v'] (find c u)
                         v' (walk s v')] ;; u' should be fully walked, but maybe not v'
                     (cond
                      (= v v') (let [[[k v :as kv] & r :as c] (dissoc c u)]
                                 (if (seq r)
                                   (recur cr (conj ncs c) simple)
                                   (recur cr ncs (conj simple kv))))
                      :else nil))))))

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

  ;; looks good
  (let [[x y z] (map lvar '[x y z])]
    (-> ((!= x 1) empty-s)
        .s
        keys
        first
        constraints))

  ;; substitution!
  (let [[x y z] (map lvar '[x y z])]
    (-> ((!= x 1) empty-s)
        ((== x 2))))

  (let [[x y z] (map lvar '[x y z])]
    (-> ((== x 2) empty-s)
        ((!= x 1))))
  
  ;; nil!
  (let [[x y z] (map lvar '[x y z])]
    (-> ((!= x 1) empty-s)
        ((== x 1))))

  ;; nil!
  (let [[x y z] (map lvar '[x y z])]
    (-> ((== x 1) empty-s)
        ((!= x 1))))

  ;; nice 400ms, no overhead for simplified constraints
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

  ;; big problem, lexical scoping, how can we add constraints w/o mutation?
  ;; will refer to the var w/o constraints

  (exist [y]
         (!= y 1)
         (foo y z) ;; we passing the y w/o constraints, this is OK now!
         (== y 1))

  ;; looking for simplified constraints in the constraint store would be slower

  ;; the order of constraints doesn't matter w/in an exist clause

  ;; we can move the constraints into the declaration of the var!

  (exist [y]
         (exist [z]
                ()
                (!= y 1)))

  ;; so we can move them to the top

  (exist [y]
         (constrain [y (!= y 1)]
                    ))

  ;; this works
  (let [y (lvar 'x)
        yc (add-constraint y 1)
        s {yc 1}]
    (constraints (first (find s y))))

  (defn test-walk [m v]
    (loop [lv v [v v'] (find m v)]
      (println v v' lv)
      (cond
       (nil? v) lv
       (identical? v' unbound) v
       (not (lvar? v')) v'
       :else (recur v' (find m v')))))

  (let [[x y] (map lvar '[x y])]
   (test-walk {x y y 1} 2))
 )

