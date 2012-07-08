(ns clojure.core.logic
  (:refer-clojure :exclude [==])
  (:use [clojure.walk :only [postwalk]])
  (:require [clojure.set :as set])
  (:import [java.io Writer]))

(def ^{:dynamic true} *occurs-check* true)
(def ^{:dynamic true} *reify-vars* true)
(def ^{:dynamic true} *locals*)
(def ^{:dynamic true} *expand-doms*)

;; =============================================================================
;; Core Protocols

;; -----------------------------------------------------------------------------
;; miniKanren Protocols

(defprotocol IUnifyTerms
  (unify-terms [u v s]))

(defprotocol IUnifyWithNil
  (unify-with-nil [v u s]))

(defprotocol IUnifyWithObject
  (unify-with-object [v u s]))

(defprotocol IUnifyWithLVar
  (unify-with-lvar [v u s]))

(defprotocol IUnifyWithLSeq
  (unify-with-lseq [v u s]))

(defprotocol IUnifyWithSequential
  (unify-with-seq [v u s]))

(defprotocol IUnifyWithMap
  (unify-with-map [v u s]))

(defprotocol IUnifyWithSet
  (unify-with-set [v u s]))

(defprotocol IReifyTerm
  (reify-term [v s]))

(defprotocol IWalkTerm
  (walk-term [v s]))

(defprotocol IOccursCheckTerm
  (occurs-check-term [v x s]))

(defprotocol IBuildTerm
  (build-term [u s]))

(defprotocol IBind
  (bind [this g]))

(defprotocol IMPlus
  (mplus [a f]))

(defprotocol ITake
  (take* [a]))

(defprotocol ISubstitutions
  (ext-no-check [this x v])
  (walk [this x] [this x wrap?]))

(defprotocol ISubstitutionsCLP
  (update [this x v]))

;; -----------------------------------------------------------------------------
;; cKanren protocols

(defprotocol IUnifyWithRefinable
  (unify-with-refinable [v u s]))

(defprotocol IUnifyWithInteger
  (unify-with-integer [v u s]))

(defprotocol IUnifyWithIntervalFD
  (unify-with-interval [v u s]))

(defprotocol IUnifyWithMultiIntervalFD
  (unify-with-multi-interval [v u s]))

(defprotocol IRunnable
  (runnable? [c s]))

(defprotocol IRefinable
  (refinable? [x]))

(defprotocol IRefine
  (refine [x v]))

(extend-type Object
  IRefinable
  (refinable? [_] false))

(defprotocol IConstraintStore
  (addc [this c])
  (updatec [this c s])
  (runc [this c])
  (constraints [this x]))

;; TODO: think about breaking apart and providing default with-id, id impl?

(defprotocol IWithConstraintId
  (with-id [this id]))

(extend-type Object
  IWithConstraintId
  (with-id [this id] this))

(defprotocol IConstraintId
  (id [this]))

(extend-type Object
  IConstraintId
  (id [this] nil))

(defprotocol IStorableConstraint
  (proc [this]))

(defprotocol IConstraintOp
  (rator [this])
  (rands [this]))

(defprotocol IRelevant
  (relevant? [this s] [this x s]))

;; TODO: add IRelevantVar ?

(defprotocol IReifiableConstraint
  (reifiable? [this])
  (reifyc [this v r]))

(extend-type Object
  IReifiableConstraint
  (reifiable? [this] false))

(defprotocol IEnforceableConstraint
  (enforceable? [c]))

(extend-type Object
  IEnforceableConstraint
  (enforceable? [x] false))

;; TODO: ICLPSet, half the below could be moved into this

(defprotocol IInterval
  (lb [this])
  (ub [this])
  (bounds [this]))

(defprotocol IIntervals
  (intervals [this]))

(defprotocol ISortedDomain
  (drop-one [this])
  (drop-before [this n])
  (keep-before [this n]))

(defprotocol IFiniteDomain
  (domain? [this])
  (member? [this that])
  (disjoint? [this that])
  (intersects? [this that])
  (subsumes? [this that]))

(defprotocol IIntersection
  (intersection [this that]))

(defprotocol IDifference
  (difference [this that]))

(extend-type Object
  IFiniteDomain
  (domain? [x] false))

(defprotocol IForceAnswerTerm
  (-force-ans [v x]))

;; =============================================================================
;; Pair

(defprotocol IPair
  (lhs [this])
  (rhs [this]))

(deftype Pair [lhs rhs]
  clojure.lang.Counted
  (count [_] 2)
  clojure.lang.Indexed
  (nth [_ i] (case i
                   0 lhs
                   1 rhs
                   (throw (IndexOutOfBoundsException.))))
  (nth [_ i not-found] (case i
                             0 lhs
                             1 rhs
                             not-found))
  IPair
  (lhs [_] lhs)
  (rhs [_] rhs)
  java.util.Map$Entry
  (getKey [_] lhs)
  (getValue [_] rhs)
  Object
  (toString [_]
    (str "(" lhs " . " rhs ")"))
  (equals [_ o]
    (if (instance? Pair o)
      (let [^Pair o o]
        (and (= lhs (.lhs o))
             (= rhs (.rhs o))))
      false)))

(defn- ^Pair pair [lhs rhs]
  (Pair. lhs rhs))

(defmethod print-method Pair [x ^Writer writer]
  (let [^Pair x x]
    (.write writer (str "(" (.lhs x) " . " (.rhs x) ")"))))

;; =============================================================================
;; Constraint Store

(declare lvar? interval multi-interval)

(defn interval-< [i j]
  (< (ub i) (lb j)))

(defn interval-> [i j]
  (> (lb i) (ub j)))

(declare domain)

(deftype FiniteDomain [s min max]
  IInterval
  (lb [_] min)
  (ub [_] max)
  (bounds [_] (pair min max))
  ISortedDomain
  (drop-one [_]
    (let [s (disj s min)]
      (when (pos? (count s))
        (FiniteDomain. s (first s) max))))
  (drop-before [_ n]
    (apply sorted-set (drop-while #(< % n)) s))
  (keep-before [this n]
    (apply sorted-set (take-while #(< % n)) s))
  IFiniteDomain
  (domain? [_] true)
  (member? [this that]
    (if (integer? that)
      (if (s that)
        true
        false)
      (member? s that)))
  (disjoint? [this that]
    (if (integer? that)
      (not (member? this that))
      (disjoint? this that)))
  IIntersection
  (intersection [this that]
    (if (integer? that)
      (when (member? this that) that)
      (intersection this that)))
  IDifference
  (difference [this that]
    (if (integer? that)
      (domain (disj s that))
      (difference that this)))
  IIntervals
  (intervals [_] (seq s)))

(defn domain [& args]
  (let [s (into (sorted-set) args)]
    (FiniteDomain. s (first s) (first (rseq s)))))

(declare interval? difference* intersection*)

(defmacro extend-to-fd [t]
  `(extend-type ~t
     IInterval
     (~'lb [this#] this#)
     (~'ub [this#] this#)
     (~'bounds [this#] (pair this# this#))
     ISortedDomain
     (~'drop-one [this#]
       nil)
     (~'drop-before [this# n#]
       (when (>= this# n#)
         this#))
     (~'keep-before [this# n#]
       (when (< this# n#)
         this#))
     IFiniteDomain
     (~'domain? [this#] true)
     (~'member? [this# that#]
       (if (integer? that#)
         (= this# that#)
         (member? that# this#)))
     (~'disjoint? [this# that#]
       (if (integer? that#)
         (not= this# that#)
         (disjoint? that# this#)))
     IIntersection
     (~'intersection [this# that#]
       (cond
        (integer? that#) (when (= this# that#)
                           this#)
        (interval? that#) (intersection that# this#)
        :else (intersection* this# that#)))
     IDifference
     (~'difference [this# that#]
       (cond
        (integer? that#) (if (= this# that#)
                           nil
                           this#)
        (interval? that#) (difference that# this#)
        :else (difference* this# that#)))
     IIntervals
     (~'intervals [this#]
       (list this#))))

(extend-to-fd java.lang.Byte)
(extend-to-fd java.lang.Short)
(extend-to-fd java.lang.Integer)
(extend-to-fd java.lang.Long)
(extend-to-fd java.math.BigInteger)
(extend-to-fd clojure.lang.BigInt)

(declare interval interval?)

(deftype IntervalFD [_lb _ub]
  Object
  (equals [_ o]
    (if (instance? IntervalFD o)
      (let [^IntervalFD o o]
        (and (= _lb (._lb o))
             (= _ub (._ub o))))
      false))
  (toString [this]
    (pr-str this))
  IRefinable
  (refinable? [_] true)
  IRefine
  (refine [this other] (intersection this other))
  IInterval
  (lb [_] _lb)
  (ub [_] _ub)
  (bounds [_] (pair _lb _ub))
  ISortedDomain
  (drop-one [_]
    (let [nlb (inc _lb)]
      (when (<= nlb _ub)
        (interval nlb _ub))))
  (drop-before [this n]
    (cond
     (= n _ub) n
     (< n _lb) this
     (> n _ub) nil
     :else (interval n _ub)))
  (keep-before [this n]
    (cond
     (<= n _lb) nil
     (> n _ub) this
     :else (interval _lb (dec n))))
  IFiniteDomain
  (domain? [_] true)
  (member? [this that]
    (cond
     (integer? that)
     (and (>= that _lb) (<= that _ub))
     
     (interval? that)
     (let [i this
           j that
           [imin imax] (bounds i)
           [jmin jmax] (bounds j)]
       (and (>= jmin imin)
            (<= jmax imax)))
     
     :else (member? that this)))
  (disjoint? [this that]
    (cond
     (integer? that)
     (not (member? this that))

     (interval? that)
     (let [i this
           j that
           [imin imax] (bounds i)
           [jmin jmax] (bounds j)]
       (or (> imin jmax)
           (< imax jmin)))

     :else (disjoint? that this)))
  IIntersection
  (intersection [this that]
    (cond
     (integer? that)
     (if (member? this that)
       that
       nil)

     (interval? that)
     (let [i this j that
           imin (lb i) imax (ub i)
           jmin (lb j) jmax (ub j)]
       (cond
        (< imax jmin) nil
        (< jmax imin) nil
        (and (<= imin jmin)
             (>= imax jmax)) j
        (and (<= jmin imin)
             (>= jmax imax)) i
        (and (<= imin jmin)
             (<= imax jmax)) (interval jmin imax)
        (and (<= jmin imin)
             (<= jmax imax)) (interval imin jmax)
        :else (throw (Error. (str "Interval intersection not defined " i " " j)))))

     :else (intersection* this that)))
  IDifference
  (difference [this that]
    (cond
     (integer? that)
     (cond
      (= _lb that) (interval (inc _lb) _ub)
      (= _ub that) (interval _lb (dec _ub))
      :else (if (member? this that)
              (multi-interval (interval _lb (dec that))
                              (interval (inc that) _ub))
              this))
     
     (interval? that)
     (let [i this j that
           imin (lb i) imax (ub i)
           jmin (lb j) jmax (ub j)]
       (cond
        (> jmin imax) i
        (and (<= jmin imin)
             (>= jmax imax)) nil
        (and (< imin jmin)
             (> imax jmax)) (multi-interval (interval imin (dec jmin))
             (interval (inc jmax) imax))
        (and (< imin jmin)
             (<= jmin imax)) (interval imin (dec jmin))
        (and (> imax jmax)
             (<= jmin imin)) (interval (inc jmax) imax)
        :else (throw (Error. (str "Interval difference not defined " i " " j)))))
     
     :else (difference* this that)))
  IIntervals
  (intervals [this]
    (list this)))

(defn interval? [x]
  (instance? IntervalFD x))

(defmethod print-method IntervalFD [x ^Writer writer]
  (.write writer (str "<interval:" (lb x) ".." (ub x) ">")))

(defn ^IntervalFD interval
  ([ub] (IntervalFD. 0 ub))
  ([lb ub]
     (if (zero? (- ub lb))
       ub
       (IntervalFD. lb ub))))

(defn intersection* [is js]
  (loop [is (seq (intervals is)) js (seq (intervals js)) r []]
    (if (and is js)
      (let [i (first is)
            j (first js)]
        (cond
         (interval-< i j) (recur (next is) js r)
         (interval-> i j) (recur is (next js) r)
         :else
         (let [[imin imax] (bounds i)
               [jmin jmax] (bounds j)]
           (cond
            (<= imin jmin)
            (cond
             ;; |----|   i   |-----|   i
             ;; |-----|  j     |-----| j
             (< imax jmax)
             (recur (next is)
                    (cons (interval (inc imax) jmax) (next js))
                    (conj r (interval jmin imax)))
 
             ;; |-----|  i   |-----|   i
             ;; |----|   j    |---|    j
             (> imax jmax)
             (recur (cons (interval (inc jmax) imax) (next is))
                    (next js)
                    (conj r j))
                  
             ;; |-----|  i
             ;;  |----|  j
             :else
             (recur (next is) (next js)
                    (conj r (interval jmin jmax))))

            (> imin jmin)
            (cond
             ;;  |-----| i
             ;; |-----|  j
             (> imax jmax)
             (recur (cons (interval (inc jmax) imax) (next is))
                    (next js)
                    (conj r (interval imin jmax)))

             ;;  |---|   i
             ;; |-----|  j
             (< imax jmax)
             (recur is (cons (interval (inc imax) jmax) (next js))
                    (conj r i))

             ;;  |----|  i
             ;; |-----|  j
             :else
             (recur (next is) (next js)
                    (conj r (interval imin imax))))))))
      (apply multi-interval r))))

(defn difference* [is js]
    (loop [is (seq (intervals is)) js (seq (intervals js)) r []]
      (if is
        (if js
          (let [i (first is)
                j (first js)]
            (cond
             (interval-< i j) (recur (next is) js (conj r i))
             (interval-> i j) (recur is (next js) r)
             :else
             (let [[imin imax] (bounds i)
                   [jmin jmax] (bounds j)]
               (cond
                (< imin jmin)
                (cond
                 ;; |-----|  i
                 ;;  |---|   j
                 (< jmax imax)
                 (recur (cons (interval (inc jmax) imax) (next is))
                        (next js)
                        (conj r (interval imin (dec jmin))))

                 ;; |-----|  i
                 ;;  |-----| j
                 (> jmax imax)
                 (recur (next is)
                        (cons (interval (inc imax) jmax) (next js))
                        (conj r (interval imin (dec jmin))))

                 ;; |-----|  i
                 ;;  |----|  j
                 :else
                 (recur (next is) (next js)
                        (conj r (interval imin (dec jmin)))))
                (>= imin jmin)
                (cond
                 ;;  |---|   i
                 ;; |-----|  j
                 (< imax jmax)
                 (recur (next is)
                        (cons (interval (inc imax) jmax) (next js))
                        r)

                 ;;  |-----| i
                 ;; |-----|  j
                 (> imax jmax)
                 (recur (cons (interval (inc jmax) imax) (next is))
                        (next js)
                        r)

                 ;;  |----|  i
                 ;; |-----|  j
                 :else (recur (next is) (next js)
                              r))))))
          (apply multi-interval (into r is)))
        (apply multi-interval r))))

(declare normalize-intervals singleton-dom?)

(deftype MultiIntervalFD [min max is]
  Object
  (equals [this j]
    (if (instance? MultiIntervalFD j)
      (let [i this
            [jmin jmax] (bounds j)]
        (if (and (= min jmin) (= max jmax))
          (let [is (normalize-intervals is)
                js (normalize-intervals (intervals j))]
            (= is js))
          false))
      false))
  IRefinable
  (refinable? [_] true)
  IRefine
  (refine [this other] (intersection this other))
  IInterval
  (lb [_] min)
  (ub [_] max)
  (bounds [_] (pair min max))
  ISortedDomain
  (drop-one [_]
    (let [i (first is)]
      (if (singleton-dom? i)
        (let [nis (rest is)]
          (MultiIntervalFD. (lb (first nis)) max nis))
        (let [ni (drop-one i)]
          (MultiIntervalFD. (lb ni) max (cons ni (rest is)))))))
  (drop-before [_ n]
    )
  (keep-before [_ n]
    )
  IFiniteDomain
  (member? [this that]
    (if (disjoint? (interval (lb this) (ub this))
                   (interval (lb that) (ub that)))
    false
    (let [d0 (intervals this)
          d1 (intervals that)]
      (loop [d0 d0 d1 d1 r []]
        (if (nil? d0)
          true)))))
  (disjoint? [this that]
    (if (disjoint? (interval (lb this) (ub this))
                   (interval (lb that) (ub that)))
      true
      (let [d0 (intervals this)
            d1 (intervals that)]
        (loop [d0 d0 d1 d1]
          (if (nil? d0)
            true
            (let [i (first d0)
                  j (first d1)]
              (cond
               (or (interval-< i j) (disjoint? i j)) (recur (next d0) d1)
               (interval-> i j) (recur d0 (next d1))
               :else false)))))))
  IIntersection
  (intersection [this that]
    (intersection* this that))
  IDifference
  (difference [this that]
    (difference* this that))
  IIntervals
  (intervals [this]
    (seq is)))

;; union where possible
(defn normalize-intervals [is]
  (reduce (fn [r i]
            (if (zero? (count r))
              (conj r i)
              (let [j (peek r)
                    jmax (ub j)
                    imin (lb i)]
                (if (<= (dec imin) jmax)
                  (conj (pop r) (interval (lb j) (ub i)))
                  (conj r i)))))
          [] is))

(defn multi-interval
  ([i0] i0)
  ([i0 i1]
     (let [is [i0 i1]]
       (MultiIntervalFD. (reduce min (map lb is)) (reduce max (map ub is)) is)))
  ([i0 i1 & ir]
     (let [is (into [] (concat (list i0 i1) ir))]
       (MultiIntervalFD. (reduce min (map lb is)) (reduce max (map ub is)) is))))

(defmethod print-method MultiIntervalFD [x ^Writer writer]
  (.write writer (str "<intervals:" (apply pr-str (.is x)) ">")))

(defn var-rands [c]
  (into [] (filter lvar? (rands c))))

(defn vars-to-remove [c s]
  (let [purge (atom true)
        vs (doall
            (filter (fn [x]
                      (when (lvar? x)
                        (let [v (walk s x)
                              remove? (not (relevant? c v s))]
                          (when-not remove?
                            (reset! purge false))
                          remove?)))
                    (rands c)))]
    (pair @purge vs)))

(deftype ConstraintStore [km cm cid running]
  IConstraintStore
  (addc [this c]
    (let [vars (var-rands c)
          c (with-id c cid)
          ^ConstraintStore cs (reduce (fn [cs v] (assoc cs v c)) this vars)]
      (ConstraintStore. (.km cs) (.cm cs) (inc cid) running)))
  (updatec [this c s]
    (let [id (id c)
          oc (get cm id)
          [purge? vs] (vars-to-remove c s)
          nkm (reduce (fn [m v]
                        (let [kcs (disj (get m v) id)]
                          (if (empty? kcs)
                            (dissoc m v)
                            (assoc m v kcs))))
                      km vs)
          ncm (if purge?
                (dissoc cm id)
                cm)]
      (ConstraintStore. nkm ncm cid (disj running id))))
  (runc [this c]
    (ConstraintStore. km cm cid (conj running (id c))))
  (constraints [this x]
    (map cm (get km x)))
  clojure.lang.Counted
  (count [this]
    (count cm))
  ;; TODO: do no expose a map interface
  clojure.lang.Associative
  (assoc [this k v]
    (when-not (lvar? k)
      (throw (Error. (str "constraint store assoc expected logic var key: " k))))
    (let [nkm (update-in km [k] (fnil (fn [s] (conj s cid)) #{}))
          ncm (assoc cm cid v)]
      (ConstraintStore. nkm ncm cid running)))
  clojure.lang.ILookup
  (valAt [this k]
    (when-let [ids (get km k)]
      (map cm (remove running ids))))
  (valAt [this k not-found]
    (if-let [v (.valAt this k)]
      v
      not-found)))

(defn ^ConstraintStore make-cs []
  (ConstraintStore.
   clojure.lang.PersistentHashMap/EMPTY
   clojure.lang.PersistentHashMap/EMPTY 0
   #{}))

;; =============================================================================
;; Substitutions

(declare empty-s)
(declare choice)
(declare lvar)
(declare lvar?)
(declare pair)
(declare lcons)
(declare run-constraints*)

(defn occurs-check [s u v]
  (let [v (walk s v)]
    (occurs-check-term v u s)))

(defn ext [s u v]
  (if (and *occurs-check* (occurs-check s u v))
    nil
    (ext-no-check s u v)))

(defn walk* [s v]
  (let [v (walk s v)]
    (walk-term v s)))

(defn unify [s u v]
  (if (identical? u v)
    s
    (let [u (walk s u true)
          v (walk s v true)]
      (if (identical? u v)
        s
        (unify-terms u v s)))))

(def unbound-names
  (let [r (range 100)]
    (zipmap r (map (comp symbol str) (repeat "_.") r))))

(defn reify-lvar-name [s]
  (let [c (count s)]
    (if (< c 100)
      (unbound-names c)
      (symbol (str "_." (count s))))))

(defn -reify* [s v]
  (let [v (walk s v)]
    (reify-term v s)))

(defn -reify [s v]
  (let [v (walk* s v)]
    (walk* (-reify* empty-s v) v)))

(defn build [s u]
  (build-term u s))

(deftype Refinable [v lvar])

(deftype Substitutions [s l cs]
  Object
  (equals [this o]
    (or (identical? this o)
        (and (.. this getClass (isInstance o))
             (= s ^clojure.lang.PersistentHashMap (.s ^Substitutions o)))))

  ;; TODO: prn doesn't work anymore on empty-s, why not?
  (toString [_] (str s))

  clojure.lang.Counted
  (count [this] (count s))

  ISubstitutions
  (ext-no-check [this u v]
    (Substitutions. (assoc s u v)
                    (cons (pair u v) l)
                    cs))

  (walk [this v]
    (walk this v false))

  (walk [this v wrap?]
    (loop [lv v [v vp] (find s v)]
      (cond
       (nil? v) lv
       (and wrap? (refinable? vp)) (if (lvar? lv) (Refinable. vp lv) vp)
       (not (lvar? vp)) vp
       :else (recur vp (find s vp)))))

  ISubstitutionsCLP
  (update [this x v]
    ((run-constraints* (if (lvar? v) [x v] [x]) cs)
     (if *occurs-check*
       (ext this x v)
       (ext-no-check this x v))))

  IBind
  (bind [this g]
    (g this))
  IMPlus
  (mplus [this f]
    (choice this f))
  ITake
  (take* [this] this))

(defn- ^Substitutions make-s
  ([] (Substitutions. clojure.lang.PersistentHashMap/EMPTY () (make-cs)))
  ([m] (Substitutions. m () (make-cs)))
  ([m l] (Substitutions. m l (make-cs)))
  ([m l cs] (Substitutions. m l cs)))

(def ^Substitutions empty-s (make-s))
(def empty-f (fn []))

(defn- subst? [x]
  (instance? Substitutions x))

(defn ^Substitutions to-s [v]
  (let [s (reduce (fn [m [k v]] (assoc m k v)) clojure.lang.PersistentHashMap/EMPTY v)
        l (reduce (fn [l [k v]] (cons (Pair. k v) l)) '() v)]
    (make-s s l (make-cs))))

;; =============================================================================
;; Logic Variables

(deftype LVar [name hash meta]
  clojure.lang.IObj
  (meta [this]
    meta)
  (withMeta [this new-meta]
    (LVar. name hash meta))
  Object
  (toString [_] (str "<lvar:" name ">"))
  (equals [this o]
    (and (.. this getClass (isInstance o))
         (let [^LVar o o]
           (identical? name (.name o)))))
  (hashCode [_] hash)
  IUnifyTerms
  (unify-terms [u v s]
    (unify-with-lvar v u s))
  IUnifyWithNil
  (unify-with-nil [v u s]
    (ext-no-check s v u))
  IUnifyWithObject
  (unify-with-object [v u s]
    (ext s v u))
  IUnifyWithLVar
  (unify-with-lvar [v u s]
    (ext-no-check s u v))
  IUnifyWithLSeq
  (unify-with-lseq [v u s]
    (ext s v u))
  IUnifyWithSequential
  (unify-with-seq [v u s]
    (ext s v u))
  IUnifyWithMap
  (unify-with-map [v u s]
    (ext s v u))
  IUnifyWithSet
  (unify-with-set [v u s]
    (ext s v u))
  IUnifyWithRefinable
  (unify-with-refinable [v u s]
    (let [^Refinable u u]
      (ext-no-check s v (.lvar u))))
  IReifyTerm
  (reify-term [v s]
    (if *reify-vars*
      (ext s v (reify-lvar-name s))
      (ext s v (:name meta))))
  IWalkTerm
  (walk-term [v s] v)
  IOccursCheckTerm
  (occurs-check-term [v x s] (= (walk s v) x))
  IBuildTerm
  (build-term [u s]
    (let [^Substitutions s s
          m (.s s)
          l (.l s)
          cs (.cs s)
          lv (lvar 'ignore) ]
      (if (contains? m u)
        s
        (make-s (assoc m u lv)
                (cons (Pair. u lv) l)
                cs)))))

(defn ^LVar lvar
  ([]
     (let [name (str (. clojure.lang.RT (nextID)))]
       (LVar. name (.hashCode name) nil)))
  ([name]
     (let [oname name
           name (str name "_" (. clojure.lang.RT (nextID)))]
       (LVar. name (.hashCode name) nil)))
  ([name cs]
     (let [oname name
           name (str name "_" (. clojure.lang.RT (nextID)))]
       (LVar. name (.hashCode name) cs))))

(defmethod print-method LVar [x ^Writer writer]
  (.write writer (str "<lvar:" (.name ^LVar x) ">")))

(defn lvar? [x]
  (instance? LVar x))

;; =============================================================================
;; LCons

(defmacro umi
  [& args]
  (if (resolve 'unchecked-multiply-int)
    `(unchecked-multiply-int ~@args)
    `(unchecked-multiply ~@args)))

(defmacro uai
  [& args]
  (if (resolve 'unchecked-add-int)
    `(unchecked-add-int ~@args)
    `(unchecked-add ~@args)))

(defprotocol LConsSeq
  (lfirst [this])
  (lnext [this]))

;; TODO: clean up the printing code

(defprotocol LConsPrint
  (toShortString [this]))

(declare lcons?)

(deftype LCons [a d ^{:unsynchronized-mutable true :tag int} cache meta]
  clojure.lang.IObj
  (meta [this]
    meta)
  (withMeta [this new-meta]
    (LCons. a d cache new-meta))
  LConsSeq
  (lfirst [_] a)
  (lnext [_] d)
  LConsPrint
  (toShortString [this]
    (cond
     (.. this getClass (isInstance d)) (str a " " (toShortString d))
     :else (str a " . " d )))
  Object
  (toString [this] (cond
                    (.. this getClass (isInstance d))
                      (str "(" a " " (toShortString d) ")")
                    :else (str "(" a " . " d ")")))
  (equals [this o]
    (or (identical? this o)
        (and (.. this getClass (isInstance o))
             (loop [me this
                    you o]
               (cond
                (nil? me) (nil? you)
                (lvar? me) true
                (lvar? you) true
                (and (lcons? me) (lcons? you))
                  (let [mef  (lfirst me)
                        youf (lfirst you)]
                    (and (or (= mef youf)
                             (lvar? mef)
                             (lvar? youf))
                         (recur (lnext me) (lnext you))))
                :else (= me you))))))

  (hashCode [this]
    (if (= cache -1)
      (do
        (set! cache (uai (umi (int 31) (clojure.lang.Util/hash d))
                         (clojure.lang.Util/hash a)))
        cache)
      cache))
  IUnifyTerms
  (unify-terms [u v s]
    (unify-with-lseq v u s))
  IUnifyWithNil
  (unify-with-nil [v u s] false)
  IUnifyWithObject
  (unify-with-object [v u s] false)
  IUnifyWithLSeq
  (unify-with-lseq [v u s]
    (loop [u u v v s s]
      (if (lvar? u)
        (unify s u v)
        (cond
         (lvar? v) (unify s v u)
         (and (lcons? u) (lcons? v))
           (if-let [s (unify s (lfirst u) (lfirst v))]
             (recur (lnext u) (lnext v) s)
             false)
         :else (unify s u v)))))
  IUnifyWithSequential
  (unify-with-seq [v u s]
    (unify-with-lseq u v s))
  IUnifyWithMap
  (unify-with-map [v u s] false)
  IUnifyWithSet
  (unify-with-set [v u s] false)
  IReifyTerm
  (reify-term [v s]
    (loop [v v s s]
      (if (lcons? v)
        (recur (lnext v) (-reify* s (lfirst v)))
        (-reify* s v))))
  ;; TODO: no way to make this non-stack consuming w/o a lot more thinking
  ;; we could use continuation passing style and trampoline
  IWalkTerm
  (walk-term [v s]
    (lcons (walk* s (lfirst v))
           (walk* s (lnext v))))
  IOccursCheckTerm
  (occurs-check-term [v x s]
    (loop [v v x x s s]
      (if (lcons? v)
        (or (occurs-check s x (lfirst v))
            (recur (lnext v) x s))
        (occurs-check s x v))))
  IBuildTerm
  (build-term [u s]
    (loop [u u s s]
      (if (lcons? u)
        (recur (lnext u) (build s (lfirst u)))
        (build s u)))))

(defmethod print-method LCons [x ^Writer writer]
  (.write writer (str x)))

(defn lcons
  "Constructs a sequence a with an improper tail d if d is a logic variable."
  [a d]
  (if (or (coll? d) (nil? d))
    (cons a (seq d))
    (LCons. a d -1 nil)))

(defn lcons? [x]
  (instance? LCons x))

(defmacro llist
  "Constructs a sequence from 2 or more arguments, with the last argument as the
   tail. The tail is improper if the last argument is a logic variable."
  ([f s] `(lcons ~f ~s))
  ([f s & rest] `(lcons ~f (llist ~s ~@rest))))

;; =============================================================================
;; Unification

;; TODO : a lot of cascading ifs need to be converted to cond

(extend-protocol IUnifyTerms
  nil
  (unify-terms [u v s]
    (unify-with-nil v u s))

  Object
  (unify-terms [u v s]
    (unify-with-object v u s))

  clojure.lang.Sequential
  (unify-terms [u v s]
    (unify-with-seq v u s))

  clojure.lang.IPersistentMap
  (unify-terms [u v s]
    (unify-with-map v u s))

  clojure.lang.IPersistentSet
  (unify-terms [u v s]
    (unify-with-set v u s))

  Refinable
  (unify-terms [u v s]
    (unify-with-refinable v u s))

  IntervalFD
  (unify-terms [u v s]
    (unify-with-interval v u s))

  MultiIntervalFD
  (unify-terms [u v s]
    (unify-with-multi-interval v u s))

  java.lang.Byte
  (unify-terms [u v s]
    (unify-with-integer v u s))

  java.lang.Short
  (unify-terms [u v s]
    (unify-with-integer v u s))

  java.lang.Integer
  (unify-terms [u v s]
    (unify-with-integer v u s))

  java.lang.Long
  (unify-terms [u v s]
    (unify-with-integer v u s))

  java.math.BigInteger
  (unify-terms [u v s]
    (unify-with-integer v u s))

  clojure.lang.BigInt
  (unify-terms [u v s]
    (unify-with-integer v u s)))

;; -----------------------------------------------------------------------------
;; Unify nil with X

(extend-protocol IUnifyWithNil
  nil
  (unify-with-nil [v u s] s)

  Object
  (unify-with-nil [v u s] false))

;; -----------------------------------------------------------------------------
;; Unify Object with X

(extend-protocol IUnifyWithObject
  nil
  (unify-with-object [v u s] false)

  Object
  (unify-with-object [v u s]
    (if (= u v) s false))

  Refinable
  (unify-with-object [v u s]
    (unify-with-refinable u v s)))

;; -----------------------------------------------------------------------------
;; Unify LVar with X

(extend-protocol IUnifyWithLVar
  nil
  (unify-with-lvar [v u s] (ext-no-check s u v))

  Object
  (unify-with-lvar [v u s]
    (ext s u v))

  Refinable
  (unify-with-lvar [v u s]
    (let [^Refinable v v]
     (ext-no-check s u (.lvar v)))))

;; -----------------------------------------------------------------------------
;; Unify LCons with X

(extend-protocol IUnifyWithLSeq
  nil
  (unify-with-lseq [v u s] false)

  Object
  (unify-with-lseq [v u s] false)

  clojure.lang.Sequential
  (unify-with-lseq [v u s]
    (loop [u u v v s s]
      (if (seq v)
        (if (lcons? u)
          (if-let [s (unify s (lfirst u) (first v))]
            (recur (lnext u) (next v) s)
            false)
          (unify s u v))
        (if (lvar? u)
          (unify s u '())
          false)))))

;; -----------------------------------------------------------------------------
;; Unify Sequential with X

(extend-protocol IUnifyWithSequential
  nil
  (unify-with-seq [v u s] false)

  Object
  (unify-with-seq [v u s] false)

  clojure.lang.Sequential
  (unify-with-seq [v u s]
    (loop [u u v v s s]
      (if (seq u)
        (if (seq v)
          (if-let [s (unify s (first u) (first v))]
            (recur (next u) (next v) s)
            false)
          false)
        (if (seq v) false s)))))

;; -----------------------------------------------------------------------------
;; Unify IPersistentMap with X

(extend-protocol IUnifyWithMap
  nil
  (unify-with-map [v u s] false)

  Object
  (unify-with-map [v u s] false)

  clojure.lang.IPersistentMap
  (unify-with-map [v u s]
    (let [ks (keys u)]
      (loop [ks ks u u v v s s]
        (if (seq ks)
          (let [kf (first ks)
                vf (get v kf ::not-found)]
            (if (= vf ::not-found)
              false
              (if-let [s (unify s (get u kf) vf)]
                (recur (next ks) (dissoc u kf) (dissoc v kf) s)
                false)))
          (if (seq v)
            false
            s))))))

;; -----------------------------------------------------------------------------
;; Unify IPersistentSet with X

(extend-protocol IUnifyWithSet
  nil
  (unify-with-set [v u s] false)

  Object
  (unify-with-set [v u s] false)

  ;; TODO : improve speed, the following takes 890ms
  ;; 
  ;; (let [a (lvar 'a)
  ;;       b (lvar 'b)
  ;;       c (lvar 'c)
  ;;       d (lvar 'd)
  ;;       s1 #{a b 3 4 5}
  ;;       s2 #{1 2 3 c d}]
  ;;     (dotimes [_ 10]
  ;;       (time
  ;;        (dotimes [_ 1e5]
  ;;          (.s (unify empty-s s1 s2))))))
  clojure.lang.IPersistentSet
  (unify-with-set [v u s]
    (loop [u u v v ulvars [] umissing []]
      (if (seq u)
        (if (seq v)
          (let [uf (first u)]
            (if (lvar? uf)
              (recur (disj u uf) v (conj ulvars uf) umissing)
              (if (contains? v uf)
                (recur (disj u uf) (disj v uf) ulvars umissing)
                (recur (disj u uf) v ulvars (conj umissing uf)))))
          false)
        (if (seq v)
          (if (seq ulvars)
            (loop [v v vlvars [] vmissing []]
              (if (seq v)
                (let [vf (first v)]
                  (if (lvar? vf)
                    (recur (disj v vf) (conj vlvars vf) vmissing)
                    (recur (disj v vf) vlvars (conj vmissing vf))))
                (unify s (concat ulvars umissing)
                       (concat vmissing vlvars))))
            false)
          s)))))

;; -----------------------------------------------------------------------------
;; Unify Refinable with X

(defn unify-with-refinable* [u v s]
  (let [^Refinable u u]
    (if-let [r (refine (.v u) v)]
      (update s (.lvar u) r)
      false)))

(extend-protocol IUnifyWithRefinable
  nil
  (unify-with-refinable [v u s] false)

  Object
  (unify-with-refinable [v u s] false)

  IntervalFD
  (unify-with-refinable [v u s]
    (unify-with-refinable* u v s))

  MultiIntervalFD
  (unify-with-refinable [v u s]
    (unify-with-refinable* u v s))

  Refinable
  (unify-with-refinable [v u s]
    (let [^Refinable u u
          ^Refinable v v]
      (if-let [r (refine (.v u) (.v v))]
        (if-let [s (update s (.lvar u) r)]
          (ext-no-check s (.lvar v) (.lvar u))
          false)
        false))))

(defn extend-type-to-unify-with-refinable [t]
  `(extend-type ~t
     IUnifyWithRefinable
     (~'unify-with-refinable [~'v ~'u ~'s]
       (~'unify-with-refinable* ~'u ~'v ~'s))))

(defmacro extend-to-unify-with-refinable [& ts]
  `(do
     ~@(map extend-type-to-unify-with-refinable ts)))

(extend-to-unify-with-refinable
  java.lang.Byte
  java.lang.Short
  java.lang.Integer
  java.lang.Long
  java.math.BigInteger
  clojure.lang.BigInt)

;; -----------------------------------------------------------------------------
;; Unify IntervalFD with X

(extend-protocol IUnifyWithIntervalFD
  nil
  (unify-with-interval [v u s] false)

  Object
  (unify-with-interval [v u s] false)

  Refinable
  (unify-with-interval [v u s]
    (unify-with-refinable* v u s))

  IntervalFD
  (unify-with-interval [v u s]
    (if (refine u v)
      s
      false))

  MultiIntervalFD
  (unify-with-interval [v u s]
    (if (refine u v)
      s
      false)))

(defn extend-type-to-unify-with-interval [t]
  `(extend-type ~t
     IUnifyWithIntervalFD
     (~'unify-with-interval [~'v ~'u ~'s]
       (if (refine ~'u ~'v)
         ~'s
         false))))

(defmacro extend-to-unify-with-interval [& ts]
  `(do
     ~@(map extend-type-to-unify-with-interval ts)))

(extend-to-unify-with-interval
  java.lang.Byte
  java.lang.Short
  java.lang.Integer
  java.lang.Long
  java.math.BigInteger
  clojure.lang.BigInt)

;; -----------------------------------------------------------------------------
;; Unify MultiIntervalFD with X

(extend-protocol IUnifyWithMultiIntervalFD
  nil
  (unify-with-multi-interval [v u s] false)

  Object
  (unify-with-multi-interval [v u s] false)

  Refinable
  (unify-with-multi-interval [v u s]
    (unify-with-refinable* v u s))

  IntervalFD
  (unify-with-multi-interval [v u s]
    (if (refine u v)
      s
      false))

  MultiIntervalFD
  (unify-with-multi-interval [v u s]
    (if (refine u v)
      s
      false)))

(defn extend-type-to-unify-with-multi-interval [t]
  `(extend-type ~t
     IUnifyWithMultiIntervalFD
     (~'unify-with-multi-interval [~'v ~'u ~'s]
       (if (refine ~'u ~'v)
         ~'s
         false))))

(defmacro extend-to-unify-with-multi-interval [& ts]
  `(do
     ~@(map extend-type-to-unify-with-multi-interval ts)))

(extend-to-unify-with-multi-interval
  java.lang.Byte
  java.lang.Short
  java.lang.Integer
  java.lang.Long
  java.math.BigInteger
  clojure.lang.BigInt)

;; -----------------------------------------------------------------------------
;; Unify Integer with X

(extend-protocol IUnifyWithInteger
  nil
  (unify-with-integer [v u s] false)

  Object
  (unify-with-integer [v u s] false)

  java.lang.Byte
  (unify-with-integer [v u s]
    (when (= u v) s))

  java.lang.Short
  (unify-with-integer [v u s]
    (when (= u v) s))

  java.lang.Integer
  (unify-with-integer [v u s]
    (when (= u v) s))

  java.lang.Long
  (unify-with-integer [v u s]
    (when (= u v) s))

  java.math.BigInteger
  (unify-with-integer [v u s]
    (when (= u v) s))

  clojure.lang.BigInt
  (unify-with-integer [v u s]
    (when (= u v) s))

  IntervalFD
  (unify-with-integer [v u s]
    (if (refine v u)
      s
      false))

  Refinable
  (unify-with-integer [v u s]
    (unify-with-refinable* v u s)))

;; =============================================================================
;; Reification

(extend-protocol IReifyTerm
  nil
  (reify-term [v s] s)

  Object
  (reify-term [v s] s)

  clojure.lang.IPersistentCollection
  (reify-term [v s]
    (loop [v v s s]
      (if (seq v)
        (recur (next v) (-reify* s (first v)))
        s))))

;; =============================================================================
;; Walk Term

(extend-protocol IWalkTerm
  nil
  (walk-term [v s] nil)

  Object
  (walk-term [v s] v)

  clojure.lang.ISeq
  (walk-term [v s]
    (with-meta
      (map #(walk* s %) v)
      (meta v)))

  clojure.lang.IPersistentVector
  (walk-term [v s]
    (with-meta
      (loop [v v r (transient [])]
        (if (seq v)
          (recur (next v) (conj! r (walk* s (first v))))
          (persistent! r)))
      (meta v)))

  clojure.lang.IPersistentMap
  (walk-term [v s]
    (with-meta
      (loop [v v r (transient {})]
        (if (seq v)
          (let [[vfk vfv] (first v)]
            (recur (next v) (assoc! r vfk (walk* s vfv))))
          (persistent! r)))
      (meta v)))

  clojure.lang.IPersistentSet
  (walk-term [v s]
    (with-meta
      (loop [v v r #{}]
        (if (seq v)
          (recur (next v) (conj r (walk* s (first v))))
          r))
      (meta v)))

  Refinable
  (walk-term [v s]
    (.v v)))

;; =============================================================================
;; Occurs Check Term

(extend-protocol IOccursCheckTerm
  nil
  (occurs-check-term [v x s] false)

  Object
  (occurs-check-term [v x s] false)

  clojure.lang.IPersistentCollection
  (occurs-check-term [v x s]
    (loop [v v x x s s]
      (if (seq v)
        (or (occurs-check s x (first v))
            (recur (next v) x s))
        false))))

;; =============================================================================
;; Build Term

(extend-protocol IBuildTerm
  nil
  (build-term [u s] s)

  Object
  (build-term [u s] s)

  clojure.lang.ISeq
  (build-term [u s]
    (reduce build s u)))

;; =============================================================================
;; Goals and Goal Constructors

(defn composeg [g0 g1]
  (fn [a]
    (let [a (g0 a)]
      (and a
           (g1 a)))))

(defmacro composeg*
  ([g0] g0)
  ([g0 & gs]
     `(composeg
       ~g0
       (composeg* ~@gs))))

(defmacro bind*
  ([a g] `(bind ~a ~g))
  ([a g & g-rest]
     `(bind* (bind ~a ~g) ~@g-rest)))

(defmacro mplus*
  ([e] e)
  ([e & e-rest]
     `(mplus ~e (fn [] (mplus* ~@e-rest)))))

(defmacro -inc [& rest]
  `(fn -inc [] ~@rest))

(extend-type Object
  ITake
  (take* [this] this))

;; TODO: Choice always holds a as a list, can we just remove that?

(deftype Choice [a f]
  IBind
  (bind [this g]
    (mplus (g a) (-inc (bind f g))))
  IMPlus
  (mplus [this fp]
    (Choice. a (fn [] (mplus (fp) f))))
  ITake
  (take* [this]
    (lazy-seq (cons (first a) (lazy-seq (take* f))))))

(defn ^Choice choice [a f]
  (Choice. a f))

;; -----------------------------------------------------------------------------
;; MZero

(extend-protocol IBind
  nil
  (bind [_ g] nil))

(extend-protocol IMPlus
  nil
  (mplus [_ b] b))

(extend-protocol ITake
  nil
  (take* [_] '()))

;; -----------------------------------------------------------------------------
;; Unit

(extend-type Object
  IMPlus
  (mplus [this f]
    (Choice. this f)))

;; -----------------------------------------------------------------------------
;; Inc

(extend-type clojure.lang.Fn
  IBind
  (bind [this g]
    (-inc (bind (this) g)))
  IMPlus
  (mplus [this f]
    (-inc (mplus (f) this)))
  ITake
  (take* [this] (lazy-seq (take* (this)))))

;; =============================================================================
;; Syntax

(defn succeed
  "A goal that always succeeds."
  [a] a)

(defn fail
  "A goal that always fails."
  [a] nil)

(def s# succeed)

(def u# fail)

(defn updateg [u v]
  (fn [a]
    (update a u v)))

(defn update-prefix [^Substitutions a ^Substitutions ap]
  (let [l (.l a)]
    ((fn loop [lp]
       (if (identical? l lp)
         s#
         (let [[lhs rhs] (first lp)]
          (composeg
           (updateg lhs rhs)
           (loop (rest lp)))))) (.l ap))))

(defn ==
  "A goal that attempts to unify terms u and v."
  [u v]
  (fn [^Substitutions a]
    (when-let [ap (unify a u v)]
      (if (pos? (count (.cs a)))
        ((update-prefix a ap) a)
        ap))))

(defn- bind-conde-clause [a]
  (fn [g-rest]
    `(bind* ~a ~@g-rest)))

(defn- bind-conde-clauses [a clauses]
  (map (bind-conde-clause a) clauses))

(defmacro conde
  "Logical disjunction of the clauses. The first goal in
  a clause is considered the head of that clause. Interleaves the
  execution of the clauses."
  [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (-inc
        (mplus* ~@(bind-conde-clauses a clauses))))))

(defn- lvar-bind [sym]
  ((juxt identity
         (fn [s] `(lvar '~s))) sym))

(defn- lvar-binds [syms]
  (mapcat lvar-bind syms))

(defmacro fresh
  "Creates fresh variables. Goals occuring within form a logical 
  conjunction."
  [[& lvars] & goals]
  `(fn [a#]
     (-inc
      (let [~@(lvar-binds lvars)]
        (bind* a# ~@goals)))))

(declare reifyg)

(defmacro solve [& [n [x :as bindings] & goals]]
  (if (> (count bindings) 1)
    `(solve ~n [q#] (fresh ~bindings ~@goals (== q# ~bindings)))
    `(let [xs# (take* (fn []
                        ((fresh [~x]
                           ~@goals
                           (reifyg ~x))
                         empty-s)))]
       (if ~n
         (take ~n xs#)
         xs#))))

(defmacro run
  "Executes goals until a maximum of n results are found."
  [n & goals]
  `(doall (solve ~n ~@goals)))

(defmacro run*
  "Executes goals until results are exhausted."
  [& goals]
  `(run false ~@goals))

(defmacro run-nc
  "Executes goals until a maximum of n results are found. Does not 
   occurs-check."
  [& [n & goals]]
  `(binding [*occurs-check* false]
     (run ~n ~@goals)))

(defmacro run-nc*
  "Executes goals until results are exhausted. Does not occurs-check."
  [& goals]
  `(run-nc false ~@goals))

(defmacro lazy-run
  "Lazily executes goals until a maximum of n results are found."
  [& [n & goals]]
  `(solve ~n ~@goals))

(defmacro lazy-run*
  "Lazily executes goals until results are exhausted."
  [& goals]
  `(solve false ~@goals))

(defmacro all
  "Like fresh but does does not create logic variables."
  ([] `clojure.core.logic/s#)
  ([& goals] `(fn [a#] (bind* a# ~@goals))))

(defn solutions
  ([s q g]
     (take*
      ((all g
        (fn [a]
          (cons (-reify a q) '()))) s))))

;; =============================================================================
;; Debugging

(defmacro log [& s]
  "Goal for println"
  `(fn [a#]
     (println ~@s)
     a#))

(defmacro trace-s []
  "Goal that prints the current substitution"
  `(fn [a#]
     (println (str a#))
     a#))

(defn trace-lvar [a lvar]
  `(println (format "%5s = %s" (str '~lvar) (-reify ~a ~lvar))))

(defmacro trace-lvars
  "Goal for tracing the values of logic variables."
  [title & lvars]
  (let [a (gensym "a")]
    `(fn [~a]
       (println ~title)
       ~@(map (partial trace-lvar a) lvars)
       ~a)))

;; =============================================================================
;; Easy Unification

(defn- lvarq-sym? [s]
  (and (symbol? s) (= (first (str s)) \?)))

(defn- proc-lvar [lvar-expr store]
  (let [v (if-let [u (@store lvar-expr)]
            u
            (lvar lvar-expr))]
    (swap! store conj [lvar-expr v])
    v))

(defn- lcons-expr? [expr]
  (and (seq? expr) (some '#{.} (set expr))))

(declare prep*)

(defn- replace-lvar [store]
  (fn [expr]
    (if (lvarq-sym? expr)
      (proc-lvar expr store)
      (if (lcons-expr? expr)
        (prep* expr store)
        expr))))

(defn- prep*
  ([expr store] (prep* expr store false false))
  ([expr store lcons?] (prep* expr store lcons? false))
  ([expr store lcons? last?]
     (let [expr (if (and last? (seq expr))
                  (first expr)
                  expr)]
       (cond
        (lvarq-sym? expr) (proc-lvar expr store)
        (seq? expr) (if (or lcons? (lcons-expr? expr))
                      (let [[f & n] expr
                            skip (= f '.)
                            tail (prep* n store lcons? skip)]
                        (if skip
                          tail
                          (lcons (prep* f store) tail)))
                      (postwalk (replace-lvar store) expr))
        :else expr))))

(defn prep
  "Prep a quoted expression. All symbols preceded by ? will
  be replaced with logic vars."
  [expr]
  (let [lvars (atom {})
        prepped (if (lcons-expr? expr)
                  (prep* expr lvars true)
                  (postwalk (replace-lvar lvars) expr))]
    (with-meta prepped {:lvars @lvars})))

(defn unifier*
  "Unify the terms u and w."
  ([u w]
     (first
      (run* [q]
        (== u w)
        (== u q))))
  ([u w & ts]
     (apply unifier* (unifier* u w) ts)))

(defn binding-map*
  "Return the binding map that unifies terms u and w.
  u and w should prepped terms."
  ([u w]
     (let [lvars (merge (-> u meta :lvars)
                        (-> w meta :lvars))
           s (unify empty-s u w)]
       (when s
         (into {} (map (fn [[k v]]
                         [k (-reify s v)])
                       lvars)))))
  ([u w & ts]
     (apply binding-map* (binding-map* u w) ts)))

(defn unifier
  "Unify the terms u and w. Will prep the terms."
  ([u w]
     {:pre [(not (lcons? u))
            (not (lcons? w))]}
     (let [up (prep u)
           wp (prep w)]
       (unifier* up wp)))
  ([u w & ts]
     (apply unifier (unifier u w) ts)))

(defn binding-map
  "Return the binding map that unifies terms u and w.
  Will prep the terms."
  ([u w]
     {:pre [(not (lcons? u))
            (not (lcons? w))]}
     (let [up (prep u)
           wp (prep w)]
       (binding-map* up wp)))
  ([u w & ts]
     (apply binding-map (binding-map u w) ts)))

;; =============================================================================
;; Non-relational goals

;; =============================================================================
;; project

(defn- project-binding [s]
  (fn [var]
    `(~var (walk* ~s ~var))))

(defn- project-bindings [vars s]
  (reduce concat (map (project-binding s) vars)))

(defmacro project
  "Extract the values bound to the specified logic vars. Non-relational."
  [[& vars] & goals]
  (let [a (gensym "a")]
    `(fn [~a]
       (let [~@(project-bindings vars a)]
         ((fresh []
            ~@goals) ~a)))))

(defmacro pred
  "Check a predicate against the value logic var. Non-relational."
  [v f]
  `(project [~v]
     (== (~f ~v) true)))

(defmacro is
  "Set the value of a var to value of another var with the operation
   applied. Non-relational."
  [u v op]
  `(project [~v]
     (== ~u (~op ~v))))

;; =============================================================================
;; conda (soft-cut), condu (committed-choice)
;;
;; conda once a line succeeds no others are tried
;; condu a line can succeed only one time

;; TODO : conda and condu should probably understanding logging

(defprotocol IIfA
  (ifa [b gs c]))

(defprotocol IIfU
  (ifu [b gs c]))

;; TODO : if -> when

(defmacro ifa*
  ([])
  ([[e & gs] & grest]
     `(ifa ~e [~@gs]
           ~(if (seq grest)
              `(delay (ifa* ~@grest))
              nil))))

(defmacro ifu*
  ([])
  ([[e & gs] & grest]
     `(ifu ~e [~@gs]
           ~(if (seq grest)
              `(delay (ifu* ~@grest))
              nil))))

(extend-protocol IIfA
  nil
  (ifa [b gs c]
       (when c
         (force c)))

  Substitutions
  (ifa [b gs c]
       (loop [b b [g0 & gr] gs]
         (if g0
           (when-let [b (g0 b)]
             (recur b gr))
           b)))

  clojure.lang.Fn
  (ifa [b gs c]
       (-inc (ifa (b) gs c)))

  Choice
  (ifa [b gs c]
    (reduce bind b gs)))

(extend-protocol IIfU
  nil
  (ifu [b gs c]
       (when c
         (force c)))

  Substitutions
  (ifu [b gs c]
    (loop [b b [g0 & gr] gs]
      (if g0
        (when-let [b (g0 b)]
          (recur b gr))
        b)))

  clojure.lang.Fn
  (ifu [b gs c]
    (-inc (ifu (b) gs c)))

  ;; TODO: Choice always holds a as a list, can we just remove that?
  Choice
  (ifu [b gs c]
    (reduce bind (.a ^Choice b) gs)))

(defn- cond-clauses [a]
  (fn [goals]
    `((~(first goals) ~a) ~@(rest goals))))

(defmacro conda
  "Soft cut. Once the head of a clause has succeeded
  all other clauses will be ignored. Non-relational."
  [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (ifa* ~@(map (cond-clauses a) clauses)))))

(defmacro condu
  "Committed choice. Once the head (first goal) of a clause 
  has succeeded, remaining goals of the clause will only
  be run once. Non-relational."
  [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (ifu* ~@(map (cond-clauses a) clauses)))))

(defn onceo [g] (condu (g)))

;; =============================================================================
;; copy-term

(defn copy-term
  "Copies a term u into v. Non-relational."
  [u v]
  (project [u]
    (== (walk* (build empty-s u) u) v)))

;; =============================================================================
;; lvar nonlvar

(defmacro lvaro
  "Goal to test whether a logic var is ground. Non-relational."
  [v]
  `(fn [a#]
     (if (lvar? (walk a# ~v))
       a# nil)))

(defmacro nonlvaro
  "Goal to test whether a logic var is ground. Non-relational."
  [v]
  `(fn [a#]
     (if (not (lvar? (walk a# ~v)))
       a# nil)))

;; =============================================================================
;; Pattern matching

(defn- warn [& msg]
  (binding [*out* *err*]
    (apply println "WARNING:" msg)))

(declare p->term)

(defn- lcons-p? [p]
  (and (coll? p)
       (not (nil? (some '#{.} p)))))

(defn- p->llist [p]
  `(llist
    ~@(map p->term
           (remove #(contains? '#{.} %) p))))

(defn- p->term [p]
  (cond
   (= p '_) `(lvar)
   (lcons-p? p) (p->llist p)
   (and (coll? p) (not= (first p) 'quote))
     (cond
      ;; support simple expressions
      (list? p) p
      ;; preserve original collection type
      :else (let [ps (map p->term p)]
              (cond
               (instance? clojure.lang.MapEntry p) (into [] ps)
               :else (into (empty p) ps))))
   :else p))

(defn- lvar-sym? [s]
  (and (symbol? s)
       (not= s '.)
       (not (contains? *locals* s))))

(defn- extract-vars
  ([p]
     (set (cond
           (lvar-sym? p) [p]           
           (coll? p) (let [p (if (seq? p) (rest p) p)]
                       (filter lvar-sym? (flatten p)))
           :else nil)))
  ([p seen]
     (set/difference (extract-vars p) (set seen))))

(defn- fresh-expr? [cs]
  (= (first cs) `fresh))

(defn- ex
  ([vs t a]
     `(fresh [~@vs]
        (== ~t ~a)))
  ([vs t a exprs]
     (if (fresh-expr? exprs)
       `(fresh [~@vs]
          (== ~t ~a)
          ~exprs)
       `(fresh [~@vs]
          (== ~t ~a)
          ~@exprs))))

(defn- ex* [[[p a :as pa] & par] exprs seen]
  (let [t (p->term p)
        vs (extract-vars p seen)
        seen (reduce conj seen vs)]
    (cond
     (nil? pa) exprs
     (= p '_) (ex* par exprs seen)
     (empty? par) (if exprs
                    (ex vs t a exprs)
                    (ex vs t a))
     :else (let [r (ex* par exprs seen)]
             (if r
               (ex vs t a r)
               (ex vs t a))))))

(defn- all-blank? [p]
  (every? #(= % '_) p))

(defn- handle-clause [as]
  (when-not (vector? as)
    (throw (Exception. (str "Expecting vector of arguments, instead " as))))
  (fn [[p & exprs]]
    (when-not (vector? p)
      (throw (Exception. (str "Expecting vector of matches, instead " p))))
    (when-not (= (count p) (count as))
      (warn "Differing number of matches. Matching" p "against" as))
    (let [pas (partition 2 (interleave p as))
          r (ex* pas exprs #{})]
      (if (all-blank? p)
        r
        (list r)))))

(defn- handle-clauses [t as cs]
  `(~t
    ~@(doall (map (handle-clause as) cs))))

;; name-with-attributes by Konrad Hinsen, from clojure.contrib.def
(defn- name-with-attributes
  "To be used in macro definitions.
   Handles optional docstrings and attribute maps for a name to be defined
   in a list of macro arguments. If the first macro argument is a string
   it is added as a docstring to name and removed from the macro argument
   list. If afterwards the first macro argument is a map, its entries are
   added to the name's metadata map and the map is removed from the
   macro argument list. The return value is a vector containing the name
   with its extended metadata map and the list of unprocessed macro
   arguments."
  [name macro-args]
  (let [[docstring macro-args] (if (string? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [nil macro-args])
    [attr macro-args]          (if (map? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [{} macro-args])
    attr                       (if docstring
                                 (assoc attr :doc docstring)
                                 attr)
    attr                       (if (meta name)
                                 (conj (meta name) attr)
                                 attr)]
    [(with-meta name attr) macro-args]))

(declare tabled)

(defn env-locals [& syms]
  (disj (set (apply concat syms)) '_))

(defmacro defnm [t n & rest]
  (let [[n [as & cs]] (name-with-attributes n rest)]
    (binding [*locals* (env-locals as (keys &env))]
     (if-let [tabled? (-> n meta :tabled)]
       `(def ~n (tabled [~@as] ~(handle-clauses t as cs)))
       `(defn ~n [~@as] ~(handle-clauses t as cs))))))

;; =============================================================================
;; Useful goals

(defn nilo
  "A relation where a is nil"
  [a]
  (== nil a))

(defn emptyo
  "A relation where a is the empty list"
  [a]
  (== '() a))

(defn conso
  "A relation where l is a collection, such that a is the first of l 
  and d is the rest of l"
  [a d l]
  (== (lcons a d) l))

(defn firsto
  "A relation where l is a collection, such that a is the first of l"
  [l a]
  (fresh [d]
    (conso a d l)))

(defn resto
  "A relation where l is a collection, such that d is the rest of l"
  [l d]
  (fresh [a]
    (== (lcons a d) l)))

;; =============================================================================
;; Goal sugar syntax

(defmacro defne
  "Define a goal fn. Supports pattern matching. All
   patterns will be tried. See conde."
  [& rest]
  `(defnm conde ~@rest))

(defmacro matche
  "Pattern matching macro. All patterns will be tried.
  See conde."
  [xs & cs]
  (binding [*locals* (env-locals xs (keys &env))]
    (handle-clauses `conde xs cs)))

;; -----------------------------------------------------------------------------
;; defnu, defna, matcha, matchu

;; TODO: we need to rethink defna and defnu, the unification comes first
;; the *question* should come first

(defmacro defna
  "Define a soft cut goal. See conda."
  [& rest]
  `(defnm conda ~@rest))

(defmacro defnu
  "Define a committed choice goal. See condu."
  [& rest]
  `(defnm condu ~@rest))

(defmacro matcha
  "Define a soft cut pattern match. See conda."
  [xs & cs]
  (binding [*locals* (env-locals xs (keys &env))]
    (handle-clauses `conda xs cs)))

(defmacro matchu
  "Define a committed choice goal. See condu."
  [xs & cs]
  (binding [*locals* (env-locals xs (keys &env))]
    (handle-clauses `condu xs cs)))

;; =============================================================================
;; More convenient goals

(defne membero 
  "A relation where l is a collection, such that l contains x"
  [x l]
  ([_ [x . tail]])
  ([_ [head . tail]]
     (membero x tail)))

(defne appendo 
  "A relation where x, y, and z are proper collections, 
  such that z is x appended to y"
  [x y z]
  ([() _ y])
  ([[a . d] _ [a . r]] (appendo d y r)))

(declare rembero)

(defne permuteo [xl yl]
  ([() ()])
  ([[x . xs] _]
     (fresh [ys]
      (permuteo xs ys)
      (rembero x yl ys))))

;; =============================================================================
;; Rel

(defn to-stream [aseq]
  (let [aseq (drop-while #(or (nil? %) (false? %)) aseq)]
    (when (seq aseq)
      (choice (first aseq)
              (fn [] (to-stream (next aseq)))))))

(defmacro def-arity-exc-helper []
  (try
    (Class/forName "clojure.lang.ArityException")
    `(defn arity-exc-helper [~'name ~'n]
       (fn [~'& ~'args]
         (throw (clojure.lang.ArityException. ~'n (str ~'name)))))
    (catch java.lang.ClassNotFoundException e
     `(defn ~'arity-exc-helper [~'name ~'n]
        (fn [~'& ~'args]
          (throw
           (java.lang.IllegalArgumentException.
            (str "Wrong number of args (" ~'n ") passed to:" ~'name))))))))

(def-arity-exc-helper)

(defn- sym-helper [prefix n]
  (symbol (str prefix n)))

(def f-sym (partial sym-helper "f"))
(def a-sym (partial sym-helper "a"))

(defn- ->sym [& args]
  (symbol (apply str args)))

(defn- defrel-helper [name arity args]
  (let [r (range 1 (+ arity 2))
        arity-excs (fn [n] `(arity-exc-helper '~name ~n))]
    (if (seq args)
      `(do
         (def ~name
           (.withMeta
            (~'clojure.core.logic.Rel.
             '~name (atom {}) nil ~@(map arity-excs r))
            {:ns ~'*ns*}))
         (extend-rel ~name ~@args))
      `(def ~name
         (.withMeta
          (~'clojure.core.logic.Rel. '~name (atom {}) nil ~@(map arity-excs r))
          {:ns ~'*ns*})))))

(defmacro def-apply-to-helper [n]
  (let [r (range 1 (clojure.core/inc n))
        args (map a-sym r)
        arg-binds (fn [n]
                    (mapcat (fn [a]
                              `(~a (first ~'arglist)
                                   ~'arglist (next ~'arglist)))
                            (take n args)))
        case-clause (fn [n]
                      `(~n (let [~@(arg-binds (dec n))]
                            (.invoke ~'ifn ~@(take (dec n) args)
                                     (clojure.lang.Util/ret1
                                      (first ~'arglist) nil)))))]
   `(defn ~'apply-to-helper
      [~(with-meta 'ifn {:tag clojure.lang.IFn}) ~'arglist]
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
                       `(invoke [~'_ ~@args]
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
       (defmacro ~'defrel 
         "Define a relation for adding facts. Takes a name and some fields.
         Use fact/facts to add facts and invoke the relation to query it."
         [~'name ~'& ~'rest]
         (defrel-helper ~'name ~arity ~'rest)))))

(RelHelper 20)

(defn- index-sym [name arity o]
  (->sym name "_" arity "-" o "-index"))

(defn- set-sym [name arity]
  (->sym name "_" arity "-set"))

;; TODO: for arity greater than 20, we need to use rest args

(defn contains-lvar? [x]
  (some lvar? (tree-seq coll? seq x)))

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
                       `((not (clojure.core.logic/contains-lvar? (clojure.core.logic/walk* ~'a ~a)))
                         ((deref ~(index-sym name arity o)) (clojure.core.logic/walk* ~'a ~a)))))
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
                     (to-stream
                      (->> set#
                           (map (fn [cand#]
                                  (when-let [~'a (clojure.core.logic/unify ~'a [~@as] cand#)]
                                    ~'a)))))))))))))

;; TODO: Should probably happen in a transaction

(defn facts
  "Define a series of facts. Takes a vector of vectors where each vector
   represents a fact tuple, all with the same number of elements."
  ([rel [f :as tuples]] (facts rel (count f) tuples))
  ([^Rel rel arity tuples]
     (let [rel-ns (:ns (meta rel))
           rel-set (var-get (ns-resolve rel-ns (set-sym (.name rel) arity)))
           tuples (map vec tuples)]
       (swap! rel-set (fn [s] (into s tuples)))
       (let [indexes (indexes-for rel arity)]
         (doseq [[o i] indexes]
           (let [index (var-get (ns-resolve rel-ns (index-sym (.name rel) arity o)))]
             (let [indexed-tuples (map (fn [t]
                                         {(nth t (dec i)) #{t}})
                                       tuples)]
               (swap! index
                      (fn [i]
                        (apply merge-with set/union i indexed-tuples))))))))))

(defn fact
  "Add a fact to a relation defined with defrel."
  [rel & tuple]
  (facts rel [(vec tuple)]))

(defn difference-with
  "Returns a map that consists of the first map with the rest of the maps
   removed from it. When a key is found in the first map and a later map,
   the value from the later map will be combined with the value in the first
   map by calling (f val-in-first val-in-later). If this function returns nil
   then the key will be removed completely."
  [f & maps]
  (when (some identity maps)
    (let [empty-is-nil (fn [s] (if (empty? s) nil s))
          merge-entry (fn [m [k v]]
                         (if (contains? m k)
                           (if-let [nv (empty-is-nil (f (get m k) v))]
                             (assoc m k nv)
                             (dissoc m k))))
          merge-map (fn [m1 m2] (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge-map maps))))

(defn retractions
  "Retract a series of facts. Takes a vector of vectors where each vector
   represents a fact tuple, all with the same number of elements. It is not
   an error to retract a fact that isn't true."
  ([rel [f :as tuples]]
     (when f (retractions rel (count f) tuples)))
  ([^Rel rel arity tuples]
     (let [rel-ns (:ns (meta rel))
           rel-set (var-get (ns-resolve rel-ns (set-sym (.name rel) arity)))
           tuples (map vec tuples)]
       (swap! rel-set (fn [s] (remove #(some #{%} tuples) s)))
       (let [indexes (indexes-for rel arity)]
         (doseq [[o i] indexes]
           (let [index (var-get (ns-resolve rel-ns (index-sym (.name rel) arity o)))]
             (let [indexed-tuples (map (fn [t]
                                         {(nth t (dec i)) #{t}})
                                       tuples)]
               (swap! index
                      (fn [i]
                        (apply difference-with set/difference i indexed-tuples))))))))))

(defn retraction
  "Remove a fact from a relation defined with defrel."
  [rel & tuple]
  (retractions rel [(vec tuple)]))

;; =============================================================================
;; Tabling

;; See - William Byrd "Relational Programming in miniKanren:
;; Techniques, Applications, and Implementations"
;; http://scholarworks.iu.edu/dspace/bitstream/handle/2022/8777/Byrd_indiana_0093A_10344.pdf?sequence=1
;; http://code.google.com/p/iucs-relational-research/

;; -----------------------------------------------------------------------------
;; Data Structures
;; (atom []) is cache, waiting streams are PersistentVectors

(defprotocol ISuspendedStream
  (ready? [this]))

(deftype SuspendedStream [cache ansv* f]
  ISuspendedStream
  (ready? [this]
          (not= @cache ansv*)))

(defn ^SuspendedStream make-ss [cache ansv* f]
  {:pre [(instance? clojure.lang.Atom cache)
         (list? ansv*)
         (fn? f)]}
  (SuspendedStream. cache ansv* f))

(defn ss? [x]
  (instance? SuspendedStream x))

(defn to-w [s]
  (into [] s))

(defn w? [x]
  (vector? x))

(defn w-check [w sk fk]
  (loop [w w a []]
    (cond
     (nil? w) (fk)
     (ready? (first w)) (sk
                         (fn []
                           (let [^SuspendedStream ss (first w)
                                 f (.f ss)
                                 w (to-w (concat a (next w)))]
                             (if (empty? w)
                               (f)
                               (mplus (f) (fn [] w))))))
     :else (recur (next w) (conj a (first w))))))

;; -----------------------------------------------------------------------------
;; Extend Substitutions to support tabling

(defprotocol ITabled
  (-reify-tabled [this v])
  (reify-tabled [this v])
  (alpha-equiv? [this x y])
  (reuse [this argv cache start end])
  (subunify [this arg ans]))

;; CONSIDER: subunify, reify-term-tabled, extending all the necessary types to
;; them

(extend-type Substitutions
  ITabled

  (-reify-tabled [this v]
                 (let [v (walk this v)]
                   (cond
                    (lvar? v) (ext-no-check this v (lvar (count (.s this))))
                    (coll? v) (-reify-tabled
                               (-reify-tabled this (first v))
                               (next v))
                    :else this)))

  (reify-tabled [this v]
                (let [v (walk* this v)]
                  (walk* (-reify-tabled empty-s v) v)))

  (alpha-equiv? [this x y]
                (= (-reify this x) (-reify this y)))

  (reuse [this argv cache start end]
         (let [start (or start @cache)
               end   (or end [])]
           (letfn [(reuse-loop [ansv*]
                     (if (= ansv* end)
                       [(make-ss cache start
                                 (fn [] (reuse this argv cache @cache start)))]
                       (Choice. (subunify this argv
                                          (reify-tabled this (first ansv*)))
                                (fn [] (reuse-loop (rest ansv*))))))]
             (reuse-loop start))))

  (subunify [this arg ans]
            (let [arg (walk this arg)]
              (cond
               (= arg ans) this
               (lvar? arg) (ext-no-check this arg ans)
               (coll? arg) (subunify
                            (subunify this (next arg) (next ans))
                            (first arg) (first ans))
               :else this))))

;; -----------------------------------------------------------------------------
;; Waiting Stream

(extend-type clojure.lang.IPersistentVector
  IBind
  (bind [this g]
        (w-check this
                 (fn [f] (bind f g))
                 (fn [] (to-w
                         (map (fn [^SuspendedStream ss]
                                (make-ss (.cache ss) (.ansv* ss)
                                         (fn [] (bind ((.f ss)) g))))
                              this)))))
  IMPlus
  (mplus [this f]
         (w-check this
                  (fn [fp] (mplus fp f))
                  (fn []
                    (let [a-inf (f)]
                      (if (w? a-inf)
                        (to-w (concat a-inf this))
                        (mplus a-inf (fn [] this)))))))
  ITake
  (take* [this]
         (w-check this
                  (fn [f] (take* f))
                  (fn [] ()))))

(defn master [argv cache]
  (fn [a]
    (when (every? (fn [ansv]
                    (not (alpha-equiv? a argv ansv)))
                  @cache)
      (do (swap! cache conj (reify-tabled a argv))
          a))))

;; -----------------------------------------------------------------------------
;; Syntax

;; TODO: consider the concurrency implications much more closely

(defn table
  "Function to table a goal. Useful when tabling should only persist
  for the duration of a run."
  [goal]
  (let [table (atom {})]
    (fn [& args]
      (let [argv args]
        (fn [a]
          (let [key (-reify a argv)
                cache (get @table key)]
            (if (nil? cache)
              (let [cache (atom ())]
                (swap! table assoc key cache)
                ((fresh []
                   (apply goal args)
                   (master argv cache)) a))
              (reuse a argv cache nil nil))))))))

(defmacro tabled
  "Macro for defining a tabled goal. Prefer ^:tabled with the 
  defne/a/u forms over using this directly."
  [args & grest]
  `(let [table# (atom {})]
     (fn [~@args]
       (let [argv# ~args]
         (fn [a#]
           (let [key# (-reify a# argv#)
                 cache# (get @table# key#)]
             (if (nil? cache#)
               (let [cache# (atom ())]
                 (swap! table# assoc key# cache#)
                 ((fresh []
                    ~@grest
                    (master argv# cache#)) a#))
               (reuse a# argv# cache# nil nil))))))))

;; =============================================================================
;; cKanren

;; See - Claire Alvis, Dan Friedman, Will Byrd, et al
;; "cKanren - miniKanren with Constraints"
;; http://www.schemeworkshop.org/2011/papers/Alvis2011.pdf
;; http://github.com/calvis/cKanren

(defn ext-cs [cs oc s]
  (if (id oc)
    (updatec cs oc s)
    (addc cs oc)))

(defn ^Substitutions update-cs [oc]
  (fn [^Substitutions a]
    (make-s (.s a) (.l a) (ext-cs (.cs a) oc a))))

(defn process-dom [v dom]
  (fn [a]
    (cond
     (lvar? v) (if-let [a (unify a v dom)] ;; TODO: why we really return nil from unify
                 a nil)
     (member? dom v) a
     :else nil)))

(defn to-vals [dom]
  (letfn [(to-vals* [is]
            (when is
              (let [i (first is)]
                (lazy-seq
                 (cons (lb i)
                       (if-let [ni (drop-one i)]
                         (to-vals* (cons ni (next is)))
                         (to-vals* (next is))))))))]
    (to-vals* (seq (intervals dom)))))

(defn map-sum [f]
  (fn loop [ls]
    (if (empty? ls)
      (fn [a] nil)
      (conde
        [(f (first ls))]
        [(loop (rest ls))]))))

(declare force-ans)

;; TODO: handle all Clojure tree types
(extend-protocol IForceAnswerTerm
  nil
  (-force-ans [v x] s#)

  Object
  (-force-ans [v x]
    (if (integer? v)
      (updateg x v)
      s#))

  clojure.lang.Sequential
  (-force-ans [v x]
    (letfn [(loop [xs]
              (if xs
                (all
                 (force-ans (first xs))
                 (loop (next xs)))
                s#))]
      (loop (seq v))))

  ;; clojure.lang.IPersistentMap
  ;; clojure.lang.IPersistentSet

  FiniteDomain
  (-force-ans [v x]
    ((map-sum (fn [n] (updateg x n))) (to-vals v)))

  IntervalFD
  (-force-ans [v x]
    ((map-sum (fn [n] (updateg x n))) (to-vals v)))

  MultiIntervalFD
  (-force-ans [v x]
    ((map-sum (fn [n] (updateg x n))) (to-vals v))))

(defn force-ans [x]
  (fn [a]
    ((let [v (walk a x)]
       (-force-ans v x)) a)))

(defn running [^Substitutions a c]
  (make-s (.s a) (.l a) (runc (.cs a) c)))

(defn run-constraint [c]
  (fn [^Substitutions a]
    (if (runnable? c a)
      (if (relevant? c a)
        ((composeg c (update-cs c)) (running a c))
        ((update-cs c) a))
      a)))

(defn run-constraints [xcs]
  (if xcs
    (composeg
     (run-constraint (first xcs))
     (run-constraints (next xcs)))
    s#))

(defn run-constraints* [xs cs]
  (if (or (zero? (count cs))
          (nil? (seq xs)))
    s#
    (let [xcs (get cs (first xs))]
      (if (seq xcs)
        (composeg
         (run-constraints xcs)
         (run-constraints* (next xs) cs))
        (run-constraints* (next xs) cs))) ))

(defn verify-all-bound [a constrained]
  (letfn [(verify-all-bound* [a constrained]
            (when constrained
              (let [f (first constrained)]
                (if (and (lvar? f) (= f (walk a f)))
                  (throw (Exception. (str "Constrained variable " f " without domain")))
                  (recur a (next constrained))))))]
    (verify-all-bound* a (seq constrained))))

(defn enforce-constraints [x]
  (all
    (force-ans x)
    (fn [^Substitutions a]
      (let [^ConstraintStore cs (.cs a)
            constrained (keys (.km cs))]
        (verify-all-bound a constrained)
        ((onceo (force-ans constrained)) a)))))

(defn reify-constraints [v r ^ConstraintStore cs]
  (let [rcs (apply concat
              (->> (get cs v)
                   (filter reifiable?)
                   (map #(reifyc % v r))))]
    (if (empty? rcs)
      (choice (list v) empty-f)
      (choice (list `(~v :- ~@rcs)) empty-f))))

(defn reifyg [x]
  (all
   (enforce-constraints x)
   (fn [^Substitutions a]
     (let [v (walk* a x)
           r (-reify* empty-s v)]
       (if (zero? (count r))
         (choice (list v) empty-f)
         (let [v (walk* r v)]
           (reify-constraints v r (.cs a))))))))

;; =============================================================================
;; CLP(FD)

;; NOTE: aliasing FD? for solving problems like zebra - David

(extend-type Object
  IRunnable
  (runnable? [c s]
    (every? domain? (map #(walk s %) (rands c)))))

(deftype FDConstraint [proc _id _meta]
  Object
  (equals [this o]
    (if (instance? FDConstraint o)
      (let [^FDConstraint o o]
        (and (= (rator this) (rator o))
             (= (rands this) (rands o))))
      false))
  clojure.lang.IObj
  (meta [this]
    _meta)
  (withMeta [this new-meta]
    (FDConstraint. proc _id new-meta))
  clojure.lang.IFn
  (invoke [_ s]
    (proc s))
  IEnforceableConstraint
  (enforceable? [_] true)
  IReifiableConstraint
  (reifiable? [_] false)
  IConstraintOp
  (rator [_] (rator proc))
  (rands [_] (rands proc))
  IRelevant
  (relevant? [this s]
    (relevant? proc s))
  (relevant? [this x s]
    (relevant? proc x s))
  IWithConstraintId
  (with-id [this new-id] (FDConstraint. proc new-id _meta))
  IConstraintId
  (id [this] _id)
  IStorableConstraint
  (proc [this] proc))

(defn fdc [proc]
  (FDConstraint. proc nil nil))

(defn fdcg [g]
  (fn [a]
    (if (runnable? g a)
      (when-let [a (g a)]
        (if (relevant? g a)
          ((update-cs (fdc g)) a)
          a))
      ((update-cs (fdc g)) a))))

(defmethod print-method FDConstraint [x ^Writer writer]
  (let [^FDConstraint x x
        cid (if-let [cid (id x)]
             (str cid ":")
             "")]
    (.write writer (str "(" cid (rator (proc x)) " " (apply str (interpose " " (rands (proc x)))) ")"))))

(defmacro infd [& xs-and-dom]
  (let [xs (butlast xs-and-dom)
        dom (last xs-and-dom)
        domsym (gensym "dom_")]
    `(let [~domsym ~dom]
      (fresh []
        ~@(map (fn [x]
                 `(domfd ~x ~domsym))
               xs)))))

(defn domfd [x dom]
  (fn [a]
    ((process-dom (walk a x) dom) a)))

(defn walk-var [a [v b]]
  `(~b (walk ~a ~v)))

(defmacro let-dom [a vars & body]
  `(let [~@(mapcat (partial walk-var a) (partition 2 vars))]
     ~@body))

(defn singleton-dom? [x]
  (and (not (lvar? x))
       (not (refinable? x))))

(defn =fdc [u v]
  (reify
    clojure.lang.IFn
    (invoke [this s]
      (let-dom s [u du v dv]
        (let [i (intersection du dv)]
          ((composeg
            (process-dom u i)
            (process-dom v i)) s))))
    IConstraintOp
    (rator [_] `=fd)
    (rands [_] [u v])
    IRelevant
    (relevant? [this s]
      (let-dom s [u du v dv]
        (cond
         (not (singleton-dom? du)) true
         (not (singleton-dom? dv)) true
         :else false)))
    (relevant? [this x s]
      (not (singleton-dom? x)))))

(defn =fd [u v]
  (fdcg (=fdc u v)))

(defn !=fdc [u v]
  (reify 
    clojure.lang.IFn
    (invoke [this s]
      (let-dom s [u du v dv]
        (cond
         (and (singleton-dom? du)
              (singleton-dom? dv)
              (= du dv)) nil
         (disjoint? du dv) s
         (singleton-dom? du)
         (when-let [vdiff (difference dv du)]
           ((process-dom v vdiff) s))
         (singleton-dom? dv)
         (when-let [udiff (difference du dv)]
           ((process-dom u udiff) s))
         :else (when-let [udiff (difference du dv)]
                 (let [vdiff (difference dv du)]
                   ((composeg
                     (process-dom u udiff)
                     (process-dom v vdiff)) s))))))
    IConstraintOp
    (rator [_] `!=fd)
    (rands [_] [u v])
    IRelevant
    (relevant? [this s]
      (let-dom s [u du v dv]
        (not (disjoint? du dv))))
    (relevant? [this x s]
      (relevant? this s))
    IRunnable
    (runnable? [this s]
      (let-dom s [u du v dv]
        (and (domain? du) (domain? dv)
             (or (singleton-dom? du)
                 (singleton-dom? dv)
                 (not= du dv)))))))

(defn !=fd [u v]
  (fdcg (!=fdc u v)))

(defn <=fdc [u v]
  (reify 
    clojure.lang.IFn
    (invoke [this s]
      (let-dom s [u du v dv]
        (let [umin (lb du)
              vmax (ub dv)]
         ((composeg*
           (process-dom u (keep-before du (inc vmax)))
           (process-dom v (drop-before dv umin))) s))))
    IConstraintOp
    (rator [_] `<=fd)
    (rands [_] [u v])
    IRelevant
    (relevant? [this s]
      (cond
       (not (singleton-dom? (walk s u))) true
       (not (singleton-dom? (walk s v))) true
       :else false))
    (relevant? [this x s]
      (not (singleton-dom? (walk s x))))))

(defn <=fd [u v]
  (fdcg (<=fdc u v)))

(defn <fd [u v]
  (all
   (<=fd u v)
   (!=fd u v)))

;; NOTE: we could put logic right back in but then we're managing
;; the constraint in the body again which were trying to get
;; away from

(defn +fdc [u v w]
  (reify 
    clojure.lang.IFn
    (invoke [this s]
      (let-dom s [u du v dv w dw]
        (let [[wmin wmax] (bounds dw)
              [umin umax] (bounds du)
              [vmin vmax] (bounds dv)]
          ((composeg
             (process-dom w (interval (+ umin vmin) (+ umax vmax)))
             (composeg
               (process-dom u (interval (- wmin vmax) (- wmax vmin)))
               (process-dom v (interval (- wmin umax) (- wmax umin))))) s))))
    IConstraintOp
    (rator [_] `+fd)
    (rands [_] [u v w])
    IRelevant
    (relevant? [this s]
      (let-dom s [u du v dv w dw]
       (cond
        (not (singleton-dom? du)) true
        (not (singleton-dom? dv)) true
        (not (singleton-dom? dw)) true
        :else (or (= du dw) (= dv dw)))))
    (relevant? [this x s]
      (relevant? this s))))

(defn +fd [u v w]
  (fdcg (+fdc u v w)))

(defn exclude-from [dom1 a xs]
  (loop [xs xs gs []]
    (if (empty? xs)
      (reduce composeg gs)
      (let [x (first xs)
            dom2 (walk a x)]
        (if (domain? dom2)
          (recur (rest xs) (conj gs (process-dom x (difference dom2 dom1))))
          (recur (rest xs) gs))))))


(defn *fdc [u v w]
  (letfn [(safe-div [n c a]
            (if (zero? n) c (/ a n)))]
   (reify
     clojure.lang.IFn
     (invoke [this s]
       (let-dom s [u du v dv w dw]
         (let [[wmin wmax] (bounds dw)
               [umin umax] (bounds du)
               [vmin vmax] (bounds dv)
               ui (interval (safe-div vmax umin wmin)
                            (safe-div vmin umax wmax))
               vi (interval (safe-div umax vmin wmin)
                            (safe-div umin vmax wmax))
               wi (interval (* umin vmin) (* umax vmax))]
           (composeg
            (process-dom w wi)
            (composeg
             (process-dom u ui)
             (process-dom v vi))))))
     IConstraintOp
     (rator [_] `*fd)
     (rands [_] [u v w])
     IRelevant
     (relevant? [this s]
       (let-dom s [u du v dv w dw]
         (cond
          (not (singleton-dom? du)) true
          (not (singleton-dom? dv)) true
          (not (singleton-dom? dw)) true
          :else (or (= du dw) (= dv dw)))))
     (relevant? [this x s]
       (relevant? this s)))))

(defn *fd [u v w]
  (fdcg (*fdc u v w)))

;; TODO: distinctfd

;; =============================================================================
;; CLP(Tree)

(defn recover-vars [p]
  (if (empty? p)
    []
    (let [[x v] (first p)
          r (recover-vars (rest p))]
      (if (lvar? v)
        (conj r x v)
        (conj r x)))))

(deftype TreeConstraint [proc _meta]
  clojure.lang.IObj
  (meta [this]
    _meta)
  (withMeta [this new-meta]
    (TreeConstraint. proc new-meta))
  IEnforceableConstraint
  (enforceable? [_] true)
  IReifiableConstraint
  (reifiable? [_] true))

(defn oc->prefix [oc]
  (first (rands oc)))

;; TODO: unify should return the prefix sub, then can eliminate l - David

(defn prefix-subsumes? [s p pp]
  (when-let [sp (unify s p pp)]
    (identical? s sp)))

(declare !=neq)

#_(defn normalize-store [p]
  (fn [^Substitutions a]
    (loop [c (.c a) cp ()]
      (if (empty? c)
        (let [cp (ext-cs (build-oc !=neq p) cp)]
          (make-s (.s a) (.l a) cp))
        (let [oc (first c)]
          (if (= (rator oc) '!=new)
            (let [pp (oc->prefix oc)]
              (cond
               (prefix-subsumes? a pp p) a
               (prefix-subsumes? a p pp) (recur (rest c) cp)
               :else (recur (rest c) (cons oc cp))))
            (recur (rest c) (cons oc cp))))))))

(defn prefix [^Substitutions s ^Substitutions <s]
  (letfn [(prefix* [s <s]
           (if (identical? s <s)
             nil
             (cons (first s) (prefix* (rest s) <s))))]
    (prefix* (.l s) (.l <s))))

#_(defn !=neq [u v]
  (fn [a]
    (if-let [ap (unify a u v)]
      (let [p (prefix a ap)]
        (when (not (empty? p))
          ((normalize-store p) a)))
      a)))

#_(defn all-diffo [l]
  (conde
    [(== l ())]
    [(fresh (a) (== l [a]))]
    [(fresh (a ad dd)
      (== l (llist a ad dd))
      (!=c a ad)
      (all-diffo (llist a dd))
      (all-diffo (llist ad dd)))]))

(declare !=)

(defne rembero [x l o]
  ([_ [x . xs] xs])
  ([_ [y . ys] [y . zs]]
     (!= y x)
     (rembero x ys zs)))
