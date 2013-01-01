(ns clojure.core.logic
  (:refer-clojure :exclude [==])
  (:require [clojure.set :as set]
            [clojure.string :as string])
  (:import [java.io Writer]
           [java.util UUID]))

(def ^{:dynamic true} *reify-vars* true)
(def ^{:dynamic true} *locals*)

(def fk (Exception.))

;; =============================================================================
;; Utilities

(defn assoc-meta [x k v]
  (with-meta x (assoc (meta x) k v)))

(defn dissoc-meta [x k]
  (with-meta x (dissoc (meta x) k)))

(defn assoc-dom [x k v]
  (assoc x :doms (assoc (:doms x) k v)))

(defn dissoc-dom [x k]
  (assoc x :doms (dissoc (:doms x) k)))

;; =============================================================================
;; Marker Interfaces

(definterface IBindable)
(definterface ITreeTerm)
(definterface IVar)

;; =============================================================================
;; Utility Protocols

(defprotocol IUninitialized
  (-uninitialized [coll]))

;; =============================================================================
;; miniKanren Protocols

;; -----------------------------------------------------------------------------
;; Unification protocols for core Clojure types

(defprotocol IUnifyTerms
  (unify-terms [u v s]))

(defprotocol IUnifyWithRecord
  (unify-with-record [u v s]))

(definterface INonStorable)

(defn non-storable? [x]
  (instance? INonStorable x))

;; -----------------------------------------------------------------------------
;; Utility protocols

(defprotocol LConsSeq
  (lfirst [this])
  (lnext [this]))

(defprotocol LConsPrint
  (toShortString [this]))

;; -----------------------------------------------------------------------------
;; Substitution

(defprotocol ISubstitutions
  (ext-no-check [this x v])
  (walk [this x]))

;; -----------------------------------------------------------------------------
;; Protocols for terms

(defprotocol IReifyTerm
  (reify-term [v s]))

(defprotocol IWalkTerm
  (walk-term [v f]))

(defprotocol IOccursCheckTerm
  (occurs-check-term [v x s]))

(defprotocol IBuildTerm
  (build-term [u s]))

;; -----------------------------------------------------------------------------
;; Goal protocols

(defprotocol IBind
  (bind [this g]))

(defprotocol IMPlus
  (mplus [a f]))

(defprotocol ITake
  (take* [a]))

;; -----------------------------------------------------------------------------
;; soft cut & committed choice protocols

(defprotocol IIfA
  (ifa [b gs c]))

(defprotocol IIfU
  (ifu [b gs c]))

;; =============================================================================
;; Rel protocols

(defprotocol IRel
  (setfn [this arity f])
  (indexes-for [this arity])
  (add-indexes [this arity index]))

;; =============================================================================
;; Tabling protocols

(defprotocol ITabled
  (-reify-tabled [this v])
  (reify-tabled [this v])
  (reuse [this argv cache start end])
  (subunify [this arg ans]))

(defprotocol ISuspendedStream
  (ready? [this]))

;; =============================================================================
;; cKanren protocols

(defprotocol ISubstitutionsCLP
  (root-val [this x])
  (root-var [this x])
  (ext-run-cs [this x v])
  (queue [this c])
  (update-var [this x v]))

;; -----------------------------------------------------------------------------
;; Constraint Store

(defprotocol IConstraintStore
  (addc [this a c])
  (updatec [this a c])
  (remc [this a c])
  (runc [this c state])
  (constraints-for [this a x ws])
  (migrate [this x root]))

;; -----------------------------------------------------------------------------
;; Generic constraint protocols

(defprotocol IRunnable
  (runnable? [c s]))

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

(defprotocol IConstraintWatchedStores
  (watched-stores [this]))

(defprotocol IConstraintOp
  (rator [this])
  (rands [this]))

(defprotocol IRelevant
  (-relevant? [this s]))

(defprotocol IRelevantVar
  (-relevant-var? [this x]))

(defprotocol IReifiableConstraint
  (reifyc [this v r a]))

(defn reifiable? [x]
  (instance? clojure.core.logic.IReifiableConstraint x))

(defprotocol IEnforceableConstraint
  (enforceable? [c]))

(extend-type Object
  IEnforceableConstraint
  (enforceable? [x] false))

(defprotocol IUnwrapConstraint
  (unwrap [c]))

(defprotocol IMergeDomains
  (-merge-doms [a b]))

;; -----------------------------------------------------------------------------
;; Finite domain protocol types

(defprotocol IInterval
  (lb [this])
  (ub [this]))

(defprotocol IMemberCount
  (member-count [this]))

(defprotocol IIntervals
  (intervals [this]))

(defprotocol IFiniteDomain
  (domain? [this]))

(extend-protocol IFiniteDomain
  nil
  (domain? [x] false)

  Object
  (domain? [x] false))

(defprotocol ISortedDomain
  (drop-one [this])
  (drop-before [this n])
  (keep-before [this n]))

(defprotocol ISet
  (member? [this n])
  (disjoint? [this that])
  (intersection [this that])
  (difference [this that]))

;; -----------------------------------------------------------------------------
;; Tree Constraints

(defprotocol IDisunifyTerms
  (disunify-terms [u v s cs]))

(defprotocol ITreeConstraint
  (tree-constraint? [this]))

(extend-type Object
  ITreeConstraint
  (tree-constraint? [this] false))

(defprotocol IPrefix
  (prefix [this]))

(defprotocol IWithPrefix
  (with-prefix [this p]))

;; -----------------------------------------------------------------------------
;; force-ans support

;; TODO: this is really Mozart/OZ "distribute"

(defprotocol IForceAnswerTerm
  (-force-ans [v x]))

;; =============================================================================
;; Pair

(deftype Pair [lhs rhs]
  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :lhs lhs
      :rhs rhs
      not-found))

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

  java.util.Map$Entry
  (getKey [_] lhs)
  (getValue [_] rhs)

  Object
  (toString [_]
    (str "(" lhs " . " rhs ")"))

  (equals [_ o]
    (if (instance? Pair o)
      (and (= lhs (:lhs o))
           (= rhs (:rhs o)))
      false)))

(defn- pair [lhs rhs]
  (Pair. lhs rhs))

(defmethod print-method Pair [x ^Writer writer]
  (.write writer (str "(" (:lhs x) " . " (:rhs x) ")")))

;; =============================================================================
;; Constraint Store

(declare lvar? bindable? interval multi-interval)

(defn bounds [i]
  (pair (lb i) (ub i)))

(defn interval-< [i j]
  (< (ub i) (lb j)))

(defn interval-> [i j]
  (> (lb i) (ub j)))

(declare domain sorted-set->domain
         difference* intersection* disjoint?*
         unify-with-domain* finite-domain?)

;; FiniteDomain
;; -----
;; wrapper around Clojure sorted sets. Used to represent small
;; domains. Optimization when interval arithmetic provides little
;; benefit.
;;
;; s - a sorted set
;; min - the minimum value, an optimization
;; max - the maximum value, an optimization

(deftype FiniteDomain [s min max]
  Object
  (equals [this that]
    (if (finite-domain? that)
      (if (= (member-count this) (member-count that))
        (= s (:s that))
        false)
      false))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :s s
      :min min
      :max max
      not-found))

  IMemberCount
  (member-count [this] (count s))

  IInterval
  (lb [_] min)
  (ub [_] max)

  ISortedDomain
  (drop-one [_]
    (let [s (disj s min)
          c (count s)]
      (cond
       (= c 1) (first s)
       (> c 1) (FiniteDomain. s (first s) max)
       :else nil)))

  (drop-before [_ n]
    (apply domain (drop-while #(< % n) s)))

  (keep-before [this n]
    (apply domain (take-while #(< % n) s)))

  IFiniteDomain
  (domain? [_] true)

  ISet
  (member? [this n]
    (if (s n) true false))

  (disjoint? [this that]
    (cond
     (integer? that)
       (if (s that) false true)
     (instance? FiniteDomain that)
       (cond
         (< max (:min that)) true
         (> min (:max that)) true
         :else (empty? (set/intersection s (:s that))))
     :else (disjoint?* this that)))

  (intersection [this that]
    (cond
     (integer? that)
       (when (member? this that) that)
     (instance? FiniteDomain that)
       (sorted-set->domain (set/intersection s (:s that)))
     :else
       (intersection* this that)))

  (difference [this that]
    (cond
     (integer? that)
       (sorted-set->domain (disj s that))
     (instance? FiniteDomain that)
       (sorted-set->domain (set/difference s (:s that)))
     :else
       (difference* this that)))

  IIntervals
  (intervals [_] (seq s))

  IMergeDomains
  (-merge-doms [this that]
    (intersection this that)))

(defn finite-domain? [x]
  (instance? FiniteDomain x))

(defn sorted-set->domain [s]
  (let [c (count s)]
    (cond
     (zero? c) nil
     (= c 1) (first s)
     :else (FiniteDomain. s (first s) (first (rseq s))))))

(defn domain
  "Construct a domain for assignment to a var. Arguments should 
   be integers given in sorted order. domains may be more efficient 
   than intervals when only a few values are possible."
  [& args]
  (when (seq args)
    (sorted-set->domain (into (sorted-set) args))))

(defmethod print-method FiniteDomain [x ^Writer writer]
  (.write writer (str "<domain:" (string/join " " (seq (:s x))) ">")))

(declare interval?)

(defmacro extend-to-fd [t]
  `(extend-type ~t
     IMemberCount
     (~'member-count [this#] 1)

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

     ISet
     (~'member? [this# that#]
       (if (integer? that#)
         (= this# that#)
         (member? that# this#)))
     (~'disjoint? [this# that#]
       (if (integer? that#)
         (not= this# that#)
         (disjoint? that# this#)))
     (~'intersection [this# that#]
       (cond
        (integer? that#) (when (= this# that#)
                           this#)
        (interval? that#) (intersection that# this#)
        :else (intersection* this# that#)))
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

(declare interval)

;; IntervalFD
;; -----
;; Type optimized for interval arithmetic. Only stores bounds.
;;
;; _lb - lower bound
;; _ub - upper bound

(deftype IntervalFD [_lb _ub]
  Object
  (equals [_ o]
    (if (instance? IntervalFD o)
      (and (= _lb (lb o))
           (= _ub (ub o)))
      false))

  (toString [this]
    (pr-str this))

  IMemberCount
  (member-count [this] (inc (- _ub _lb)))

  IInterval
  (lb [_] _lb)
  (ub [_] _ub)

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

  ISet
  (member? [this n]
    (and (>= n _lb) (<= n _ub)))

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

     :else (disjoint?* this that)))

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
    (list this))

  IMergeDomains
  (-merge-doms [this that]
    (intersection this that)))

(defn interval? [x]
  (instance? IntervalFD x))

(defmethod print-method IntervalFD [x ^Writer writer]
  (.write writer (str "<interval:" (lb x) ".." (ub x) ">")))

(defn interval
  "Construct an interval for an assignment to a var. intervals may
   be more efficient that the domain type when the range of possiblities
   is large."
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
             (< imax jmax)
             (recur (next is)
                    (cons (interval (inc imax) jmax) (next js))
                    (conj r (interval jmin imax)))
             (> imax jmax)
             (recur (cons (interval (inc jmax) imax) (next is))
                    (next js)
                    (conj r j))
             :else
             (recur (next is) (next js)
                    (conj r (interval jmin jmax))))
            (> imin jmin)
            (cond
             (> imax jmax)
             (recur (cons (interval (inc jmax) imax) (next is))
                    (next js)
                    (conj r (interval imin jmax)))
             (< imax jmax)
             (recur is (cons (interval (inc imax) jmax) (next js))
                    (conj r i))
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
                 (< jmax imax)
                 (recur (cons (interval (inc jmax) imax) (next is))
                        (next js)
                        (conj r (interval imin (dec jmin))))
                 (> jmax imax)
                 (recur (next is)
                        (cons (interval (inc imax) jmax) (next js))
                        (conj r (interval imin (dec jmin))))
                 :else
                 (recur (next is) (next js)
                        (conj r (interval imin (dec jmin)))))
                (>= imin jmin)
                (cond
                 (< imax jmax)
                 (recur (next is)
                        (cons (interval (inc imax) jmax) (next js))
                        r)
                 (> imax jmax)
                 (recur (cons (interval (inc jmax) imax) (next is))
                        (next js)
                        r)
                 :else (recur (next is) (next js)
                              r))))))
          (apply multi-interval (into r is)))
        (apply multi-interval r))))

(defn disjoint?* [is js]
  (if (disjoint? (interval (lb is) (ub is))
                 (interval (lb js) (ub js)))
      true
      (let [d0 (intervals is)
            d1 (intervals js)]
        (loop [d0 d0 d1 d1]
          (if (nil? d0)
            true
            (let [i (first d0)
                  j (first d1)]
              (cond
               (or (interval-< i j) (disjoint? i j)) (recur (next d0) d1)
               (interval-> i j) (recur d0 (next d1))
               :else false)))))))

(declare normalize-intervals singleton-dom? multi-interval)

;; MultiIntervalFD
;; -----
;; Running difference operations on IntervalFD will result in
;; a series of intervals.
;;
;; min - minimum value of all contained intervals
;; max - maximum value of all contained intervals
;; is  - the intervals

(deftype MultiIntervalFD [min max is]
  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :is is
      :min min
      :max max
      not-found))

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

  IMemberCount
  (member-count [this]
    (reduce + 0 (map member-count is)))

  IInterval
  (lb [_] min)
  (ub [_] max)

  ISortedDomain
  (drop-one [_]
    (let [i (first is)]
      (if (singleton-dom? i)
        (let [nis (rest is)]
          (MultiIntervalFD. (lb (first nis)) max nis))
        (let [ni (drop-one i)]
          (MultiIntervalFD. (lb ni) max (cons ni (rest is)))))))

  (drop-before [_ n]
    (let [is (seq is)]
      (loop [is is r []]
        (if is
          (let [i (drop-before (first is) n)]
            (if i
              (recur (next is) (conj r i))
              (recur (next is) r)))
          (when (pos? (count r))
            (apply multi-interval r))))))

  (keep-before [_ n]
    (let [is (seq is)]
      (loop [is is r []]
        (if is
          (let [i (keep-before (first is) n)]
            (if i
              (recur (next is) (conj r i))
              (recur (next is) r)))
          (when (pos? (count r))
            (apply multi-interval r))))))

  IFiniteDomain
  (domain? [_] true)

  ISet
  (member? [this n]
    (if (some #(member? % n) is)
      true
      false))
  (disjoint? [this that]
    (disjoint?* this that))
  (intersection [this that]
    (intersection* this that))
  (difference [this that]
    (difference* this that))

  IIntervals
  (intervals [this]
    (seq is))

  IMergeDomains
  (-merge-doms [this that]
    (intersection this that)))

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
  ([] nil)
  ([i0] i0)
  ([i0 i1]
     (let [is [i0 i1]]
       (MultiIntervalFD. (reduce min (map lb is)) (reduce max (map ub is)) is)))
  ([i0 i1 & ir]
     (let [is (into [] (concat (list i0 i1) ir))]
       (MultiIntervalFD. (reduce min (map lb is)) (reduce max (map ub is)) is))))

(defmethod print-method MultiIntervalFD [x ^Writer writer]
  (.write writer (str "<intervals:" (apply pr-str (:is x)) ">")))

(defn var-rands [a c]
  (->> (rands c)
    (map #(root-var a %))
    (filter lvar?)
    (into [])))

(defn unbound-rands [a c]
  (->> (var-rands a c)
    (filter #(lvar? (root-val a %)))))

(declare add-var)

;; ConstraintStore
;; -----
;; km  - mapping logic vars to constraints ids
;; cm  - mapping constraint ids to to actual constraints
;; cid - the current constraint id, an integer, incremented
;;       everytime we add a constraint to the store
;; running - set of running constraint ids

(deftype ConstraintStore [km cm cid running]
  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :km km
      :cm cm
      :cid cid
      :running running
      not-found))

  IConstraintStore
  (addc [this a c]
    (let [vars (var-rands a c)
          c (with-id c cid)
          cs (reduce (fn [cs v] (add-var cs v c)) this vars)]
      (ConstraintStore. (:km cs) (:cm cs) (inc cid) running)))

  (updatec [this a c]
    (let [oc (cm (id c))
          nkm (if (instance? clojure.core.logic.IRelevantVar c)
                (reduce (fn [km x]
                          (if-not (-relevant-var? c x)
                            (dissoc km x)
                            km))
                        km (var-rands a oc))
                km)]
      (ConstraintStore. nkm (assoc cm (id c) c) cid running)))

  (remc [this a c]
    (let [vs (var-rands a c)
          ocid (id c)
          nkm (reduce (fn [km v]
                        (let [vcs (disj (get km v) ocid)]
                          (if (empty? vcs)
                            (dissoc km v)
                            (assoc km v vcs))))
                      km vs)
          ncm (dissoc cm ocid)]
      (ConstraintStore. nkm ncm cid running)))

  (runc [this c state]
    (if state
      (ConstraintStore. km cm cid (conj running (id c)))
      (ConstraintStore. km cm cid (disj running (id c)))))

  (constraints-for [this a x ws]
    (when-let [ids (get km (root-var a x))]
      (filter #((watched-stores %) ws) (map cm (remove running ids)))))

  (migrate [this x root]
    (let [xcs    (km x)
          rootcs (km root)
          nkm    (assoc (dissoc km x) root (into rootcs xcs))]
      (ConstraintStore. nkm cm cid running)))

  clojure.lang.Counted
  (count [this]
    (count cm)))

(defn add-var [cs x c]
  (when-not (lvar? x)
    (throw (Error. (str "constraint store assoc expected logic var key: " x))))
  (let [cm (:cm cs)
        km (:km cs)
        cid (:cid cs)
        nkm (update-in km [x] (fnil (fn [s] (conj s cid)) #{}))
        ncm (assoc cm cid c)]
    (ConstraintStore. nkm ncm cid (:running cs))))

(defn make-cs []
  (ConstraintStore. {} {} 0 #{}))

;; =============================================================================
;; SubstValue

(defrecord SubstValue [v doms]
  Object
  (toString [_]
    (str v)))

(defn subst-val? [x]
  (instance? SubstValue x))

(defn subst-val
  ([x] (SubstValue. x nil))
  ([x doms] (SubstValue. x doms))
  ([x doms _meta] (with-meta (SubstValue. x doms) _meta)))

(defmethod print-method SubstValue [x ^Writer writer]
  (.write writer (str (:v x))))

;; =============================================================================
;; Substitutions

(declare empty-s choice lvar lvar? pair lcons run-constraints*)

(defn occurs-check [s u v]
  (let [v (walk s v)]
    (occurs-check-term v u s)))

(defn ext [s u v]
  (if (and (:oc s) (occurs-check s u (if (subst-val? v) (:v v) v)))
    nil
    (ext-no-check s u v)))

(declare tree-term?)

(defn walk* [s v]
  (let [v (walk s v)]
    (walk-term v
      (fn [x]
        (let [x (walk s x)]
         (if (tree-term? x)
           (walk* s x)
           x))))))

(defn unify [s u v]
  (if (identical? u v)
    s
    (let [u  (walk s u)
          v  (walk s v)]
      ;; TODO: we can't use an identical? check here at the moment
      ;; because we add metadata on vars in walk - David
      (if (and (lvar? u) (= u v))
        s
        (if (and (not (lvar? u)) (lvar? v))
          (unify-terms v u s)
          (unify-terms u v s))))))

(def unbound-names
  (let [r (range 100)]
    (zipmap r (map (comp symbol str) (repeat "_") r))))

(defn reify-lvar-name [s]
  (let [c (count s)]
    (if (< c 100)
      (unbound-names c)
      (symbol (str "_" (count s))))))

(defn -reify* [s v]
  (let [v (walk s v)]
    (reify-term v s)))

(defn -reify
  ([s v]
     (let [v (walk* s v)]
       (walk* (-reify* empty-s v) v)))
  ([s v r]
     (let [v (walk* s v)]
       (walk* (-reify* r v) v))))

(defn build [s u]
  (build-term u s))

(declare singleton-dom? empty-s make-s)

;; Substitutions
;; -----
;; s   - persistent hashmap to store logic var bindings
;; vs  - changed var set
;; ts  - atom containing a hashmap of
;;       tabled goals -> atoms of sets containing cached answers
;; cs  - constraint store
;; cq  - for the constraint queue
;; cqs - constraint ids in the queue
;; oc  - occurs check

(deftype Substitutions [s vs ts cs cq cqs oc _meta]
  Object
  (equals [this o]
    (or (identical? this o)
        (and (.. this getClass (isInstance o))
             (= s (:s o)))))
  ;; TODO: prn doesn't work anymore on empty-s, why not?
  (toString [_] (str s))

  clojure.lang.Counted
  (count [this] (count s))

  clojure.lang.IObj
  (meta [this] _meta)
  (withMeta [this new-meta]
    (Substitutions. s vs ts cs cq cqs oc new-meta))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :s   s
      :vs  vs
      :ts  ts
      :cs  cs
      :cq  cq
      :cqs cqs
      :oc  oc
      not-found))

  clojure.lang.IPersistentCollection
  (cons [this [k v]]
    (if (lvar? k)
      (assoc this k v)
      (throw (Exception. (str "key must be a logic var")))))
  (empty [this] empty-s)
  (equiv [this o]
    (.equals this o))

  clojure.lang.Associative
  (containsKey [this k]
    (contains? #{:s :vs :cs :cq :cqs} k))
  (entryAt [this k]
    (case k
      :s   [:s s]
      :vs  [:vs vs]
      :ts  [:ts ts]
      :cs  [:cs cs]
      :cq  [:cq cq]
      :cqs [:cqs cqs]
      :oc  [:oc cqs]
      nil))
  (assoc [this k v]
    (case k
      :s   (Substitutions. v vs ts cs cq cqs oc _meta)
      :vs  (Substitutions. s  v ts cs cq cqs oc _meta)
      :ts  (Substitutions. s vs  v cs cq cqs oc _meta)
      :cs  (Substitutions. s vs ts  v cq cqs oc _meta)
      :cq  (Substitutions. s vs ts cs  v cqs oc _meta)
      :cqs (Substitutions. s vs ts cs cq   v oc _meta)
      :oc  (Substitutions. s vs ts cs cq cqs  v _meta)
      (throw (Exception. (str "Substitutions has no field for key" k)))))

  ISubstitutions
  (ext-no-check [this u v]
    (let [u (if-not (lvar? v)
              (assoc-meta u ::root true)
              u)]
      (Substitutions. (assoc s u v) (if vs (conj vs u)) ts cs cq cqs oc _meta)))

  (walk [this v]
    (if (bindable? v)
      (loop [lv v [v vp :as me] (find s v)]
        (cond
          (nil? me) lv
          
          (not (bindable? vp))
          (if (subst-val? vp)
            (let [sv (:v vp)]
              (if (= sv ::unbound)
                (with-meta v (assoc (meta vp) ::unbound true))
                sv))
            vp)
          
          :else (recur vp (find s vp))))
      v))

  ISubstitutionsCLP
  (root-val [this v]
    (if (lvar? v)
      (loop [lv v [v vp :as me] (find s v)]
        (cond
          (nil? me) lv
          (not (lvar? vp)) vp
          :else (recur vp (find s vp))))
      v))

  (root-var [this v]
    (if (lvar? v)
      (if (-> v meta ::root)
        v
        (loop [lv v [v vp :as me] (find s v)]
          (cond
            (nil? me) lv

            (not (lvar? vp))
            (if (subst-val? vp)
              (with-meta v (meta vp))
              v)

            :else (recur vp (find s vp)))))
      v))
  
  (ext-run-cs [this x v]
    (let [x  (root-var this x)
          xs (if (lvar? v)
               [x (root-var this v)]
               [x])
          s  (if oc
               (ext this x v)
               (ext-no-check this x v))]
      (when s
        ((run-constraints* xs cs ::subst) s))))

  (queue [this c]
    (let [id (id c)]
      (if-not (cqs id)
        (-> this
          (assoc :cq (conj (or cq []) c))
          (assoc :cqs (conj cqs id)))
        this)))

  (update-var [this x v]
    (assoc this :s (assoc (:s this) x v)))

  IBind
  (bind [this g]
    (g this))
  IMPlus
  (mplus [this f]
    (choice this f))
  ITake
  (take* [this] this))

(defn add-attr [s x attr attrv]
  (let [x (root-var s x)
        v (root-val s x)]
    (if (subst-val? v)
      (update-var s x (assoc-meta v attr attrv))
      (let [v (if (lvar? v) ::unbound v)]
        (ext-no-check s x (with-meta (subst-val v) {attr attrv}))))))

(defn rem-attr [s x attr]
  (let [x (root-var s x)
        v (root-val s x)]
    (if (subst-val? v)
      (let [new-meta (dissoc (meta v) attr)]
        (if (and (zero? (count new-meta)) (not= (:v v) ::unbound))
          (update-var s x (:v v))
          (update-var s x (with-meta v new-meta))))
      s)))

(defn get-attr [s x attr]
  (let [v (root-val s x)]
    (if (subst-val? v)
      (-> v meta attr))))

(defn add-dom [s x dom domv]
  (let [x (root-var s x)
        v (root-val s x)]
    (if (subst-val? v)
      (update-var s x (assoc-dom v dom domv))
      (let [v (if (lvar? v) ::unbound v)]
        (ext-no-check s x (subst-val v {dom domv}))))))

(defn update-dom [s x dom f]
  (let [x (root-var s x)
        v (root-val s x)
        v (if (lvar? v)
            (subst-val ::unbound)
            v)
        doms (:doms v)]
    (update-var s x (assoc-dom v dom (f (get doms dom))))))

(defn rem-dom [s x dom]
  (let [x (root-var s x)
        v (root-val s x)]
    (if (subst-val? v)
      (let [new-doms (dissoc (:doms v) dom)]
        (if (and (zero? (count new-doms)) (not= (:v v) ::unbound))
          (update-var s x (:v v))
          (update-var s x (assoc v :doms new-doms))))
      s)))

(defn get-dom [s x dom]
  (let [v (root-val s x)]
    (if (subst-val? v)
      (-> v :doms dom))))

(defn- make-s
  ([] (Substitutions. {} nil nil (make-cs) nil #{} true nil))
  ([m] (Substitutions. m nil nil (make-cs) nil #{} true nil))
  ([m cs] (Substitutions. m nil nil cs nil #{} true nil)))

(defn tabled-s
  ([] (tabled-s false))
  ([oc] (Substitutions. {} nil (atom {}) (make-cs) nil #{} oc nil)))

(def empty-s (make-s))
(def empty-f (fn []))

(defn subst? [x]
  (instance? Substitutions x))

(defn to-s [v]
  (let [s (reduce (fn [m [k v]] (assoc m k v)) {} v)]
    (make-s s (make-cs))))

(defn annotate [k v]
  (fn [a]
    (vary-meta a assoc k v)))

(defn merge-subst-vals [x root]
  (let [doms (loop [xd (seq (:doms x)) rd (:doms root) r {}]
               (if xd
                 (let [[xk xv] (first xd)]
                   (if-let [[_ rv] (find rd xk)]
                     (let [nd (-merge-doms xv rv)]
                       (when nd
                         (recur (next xd) (dissoc rd xk)
                           (assoc r xk nd))))
                     (recur (next xd) rd (assoc r xk xv))))
                 (merge r rd)))]
    (when doms
      (subst-val (:v root) doms
        (merge (meta x) (meta root))))))

;; =============================================================================
;; Logic Variables

(deftype LVar [name oname hash meta]
  clojure.core.logic.IVar

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :name name
      :oname oname
      not-found))

  clojure.lang.IObj
  (meta [this]
    meta)
  (withMeta [this new-meta]
    (LVar. name oname hash new-meta))

  Object
  (toString [_] (str "<lvar:" name ">"))

  (equals [this o]
    (and (instance? clojure.core.logic.IVar o)
      (identical? name (:name o))))

  (hashCode [_] hash)

  IUnifyTerms
  (unify-terms [u v s]
    (cond
      (lvar? v)
      (let [repoint (cond
                      (-> u clojure.core/meta ::unbound) [u v]
                      (-> v clojure.core/meta ::unbound) [v u]
                      :else nil)]
        (if repoint
          (let [[root other] repoint
                s (assoc s :cs (migrate (:cs s) other root))
                s (if (-> other clojure.core/meta ::unbound)
                    (when-let [nsv (merge-subst-vals
                                    (root-val s other)
                                    (root-val s root))]
                      (ext-no-check s root nsv))
                    s)]
            (when s
              (ext-no-check s other root)))
          (ext-no-check s u v)))

      (non-storable? v)
      (throw (Exception. (str v " is non-storable")))

      (not= v ::not-found)
      (if (tree-term? v)
        (ext s u v)
        (if (-> u clojure.core/meta ::unbound)
          (ext-no-check s u (assoc (root-val s u) :v v))
          (ext-no-check s u v)))
      
      :else nil))

  IReifyTerm
  (reify-term [v s]
    (if *reify-vars*
      (ext s v (reify-lvar-name s))
      (ext s v (:oname v))))

  IWalkTerm
  (walk-term [v f] (f v))

  IOccursCheckTerm
  (occurs-check-term [v x s] (= (walk s v) x))

  IBuildTerm
  (build-term [u s]
    (let [m (:s s)
          l (:l s)
          cs (:cs s)
          lv (lvar 'ignore) ]
      (if (contains? m u)
        s
        (make-s (assoc m u lv)
                (cons (Pair. u lv) l)
                cs)))))

(defn lvar
  ([]
     (let [name (str (. clojure.lang.RT (nextID)))]
       (LVar. name nil (.hashCode name) nil)))
  ([name]
     (lvar name true))
  ([name gensym]
     (let [oname name
           name (if gensym
                  (str name (. clojure.lang.RT (nextID)))
                  (str name))]
       (LVar. name oname (.hashCode name) nil))))

(defmethod print-method LVar [x ^Writer writer]
  (.write writer (str "<lvar:" (:name x) ">")))

(defn lvar? [x]
  (instance? clojure.core.logic.IVar x))

(defn lvars [n]
  (repeatedly n lvar))

(defn bindable? [x]
  (or (lvar? x)
    (instance? clojure.core.logic.IBindable x)))

;; =============================================================================
;; LCons

(declare lcons?)

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

;; TODO: clean up the printing code

(deftype LCons [a d ^{:unsynchronized-mutable true :tag int} cache meta]
  clojure.core.logic.ITreeTerm

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
    (cond
      (sequential? v)
      (loop [u u v v s s]
        (if (seq v)
          (if (lcons? u)
            (if-let [s (unify s (lfirst u) (first v))]
              (recur (lnext u) (next v) s)
              nil)
            (unify s u v))
          (if (lvar? u)
            (if-let [s (unify s u '())]
              s
              (unify s u nil))
            nil)))
      
      (lcons? v)
      (loop [u u v v s s]
        (if (lvar? u)
          (unify s u v)
          (cond
            (lvar? v) (unify s v u)
            (and (lcons? u) (lcons? v))
            (if-let [s (unify s (lfirst u) (lfirst v))]
              (recur (lnext u) (lnext v) s)
              nil)
            :else (unify s u v))))
      
      :else nil))

  IReifyTerm
  (reify-term [v s]
    (loop [v v s s]
      (if (lcons? v)
        (recur (lnext v) (-reify* s (lfirst v)))
        (-reify* s v))))

  ;; TODO: no way to make this non-stack consuming w/o a lot more thinking
  ;; we could use continuation passing style and trampoline
  IWalkTerm
  (walk-term [v f]
    (lcons (f (lfirst v))
           (f (lnext v))))

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

(defn tree-term? [x]
  (or (coll? x)
      (instance? clojure.core.logic.ITreeTerm x)))

;; =============================================================================
;; Unification

;; TODO : a lot of cascading ifs need to be converted to cond

(extend-protocol IUnifyTerms
  nil
  (unify-terms [u v s]
    (if (nil? v) s nil))

  Object
  (unify-terms [u v s]
    (if (= u v)
      s
      nil))

  clojure.lang.Sequential
  (unify-terms [u v s]
    (cond
      (sequential? v)
      (if (and (counted? u) (counted? v)
            (not= (count u) (count v)))
        nil
        (loop [u u v v s s]
          (if (seq u)
            (if (seq v)
              (if-let [s (unify s (first u) (first v))]
                (recur (next u) (next v) s)
                nil)
              nil)
            (if (seq v) nil s))))
      
      (lcons? v) (unify-terms v u s)
      :else nil))

  clojure.lang.IPersistentMap
  (unify-terms [u v s]
    (cond
      (instance? clojure.core.logic.IUnifyWithRecord v)
      (unify-with-record v u s)

      (map? v)
      (when (= (count u) (count v))
        (loop [ks (keys u) s s]
          (if (seq ks)
            (let [kf (first ks)
                  vf (get v kf ::not-found)]
              (when-not (= vf ::not-found)
                (if-let [s (unify s (get u kf) vf)]
                  (recur (next ks) s)
                  nil)))
            s)))
      :else nil)))

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

(defn walk-record-term [v f]
  (with-meta
    (loop [v v r (-uninitialized v)]
      (if (seq v)
        (let [[vfk vfv] (first v)]
          (recur (next v) (assoc r (walk-term (f vfk) f)
                                 (walk-term (f vfv) f))))
        r))
    (meta v)))

(extend-protocol IWalkTerm
  nil
  (walk-term [v f] (f nil))

  Object
  (walk-term [v f] (f v))

  clojure.lang.ISeq
  (walk-term [v f]
    (with-meta
      (map #(walk-term (f %) f) v)
      (meta v)))

  clojure.lang.IPersistentVector
  (walk-term [v f]
    (with-meta
      (loop [v v r (transient [])]
        (if (seq v)
          (recur (next v) (conj! r (walk-term (f (first v)) f)))
          (persistent! r)))
      (meta v)))

  clojure.lang.IPersistentMap
  (walk-term [v f]
    (with-meta
      ;; TODO: call empty here on v to preserve the type
      ;; we were given, we can have the transient bit
      ;; for the cases where we have a concrete Clojure map
      ;; type, and just usy empty + assoc for everything else
      (loop [v v r (transient {})]
        (if (seq v)
          (let [[vfk vfv] (first v)]
            (recur (next v) (assoc! r (walk-term (f vfk) f)
                                    (walk-term (f vfv) f))))
          (persistent! r)))
      (meta v))))

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

(defn composeg
  ([] identity)
  ([g0 g1]
     (fn [a]
       (let [a (g0 a)]
         (and a (g1 a))))))

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
  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :a a
      not-found))
  IBind
  (bind [this g]
    (mplus (g a) (fn [] (bind f g))))
  IMPlus
  (mplus [this fp]
    (Choice. a (fn [] (mplus (fp) f))))
  ITake
  (take* [this]
    (lazy-seq (cons (first a) (lazy-seq (take* f))))))

(defn choice [a f]
  (Choice. a f))

;; -----------------------------------------------------------------------------
;; MZero

(extend-protocol IBind
  nil
  (bind [_ g] nil))

(extend-protocol IMPlus
  nil
  (mplus [_ f] (f)))

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

(defn ext-run-csg [u v]
  (fn [a]
    (ext-run-cs a u v)))

(defn ==
  "A goal that attempts to unify terms u and v."
  [u v]
  (fn [a]
    (let [has-cs? (pos? (count (:cs a)))]
      (let [ap (unify (if has-cs? (assoc a :vs []) a) u v)
            vs (if has-cs? (:vs ap))
            changed? (pos? (count vs))]
        (if changed?
          ((run-constraints* vs (:cs ap) ::subst) (assoc ap :vs nil))
          ap)))))

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

(defmacro -run [oc n [x :as bindings] & goals]
  (if (> (count bindings) 1)
    `(-run ~oc ~n [q#] (fresh ~bindings ~@goals (== q# ~bindings)))
    `(let [xs# (take* (fn []
                        ((fresh [~x]
                           ~@goals
                           (reifyg ~x))
                         (tabled-s ~oc))))]
       (if ~n
         (take ~n xs#)
         xs#))))

(defmacro run
  "Executes goals until a maximum of n results are found."
  [n & goals]
  `(-run true ~n ~@goals))

(defmacro run*
  "Executes goals until results are exhausted."
  [& goals]
  `(-run true false ~@goals))

(defmacro run-nc
  "Executes goals until a maximum of n results are found. Does not 
   occurs-check."
  [& [n & goals]]
  `(-run false ~n ~@goals))

(defmacro run-nc*
  "Executes goals until results are exhausted. Does not occurs-check."
  [& goals]
  `(run-nc false ~@goals))

(defmacro all
  "Like fresh but does does not create logic variables."
  ([] `clojure.core.logic/s#)
  ([& goals] `(fn [a#] (bind* a# ~@goals))))

(defn solutions
  ([s g]
     (solutions s (lvar) g))
  ([s q g]
     (take* ((all g (reifyg q)) s))))

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
            (lvar lvar-expr false))]
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
                      (doall (walk-term expr (replace-lvar store))))
        :else expr))))

(defn prep
  "Prep a quoted expression. All symbols preceded by ? will
  be replaced with logic vars."
  [expr]
  (let [lvars (atom {})
        prepped (if (lcons-expr? expr)
                  (prep* expr lvars true)
                  (doall (walk-term expr (replace-lvar lvars))))]
    (with-meta prepped {:lvars @lvars})))

(declare fix-constraints)

(defn unifier*
  "Unify the terms u and w."
  ([u w]
     (let [init-s (reduce
                    (fn [s [vs cs]]
                      (let [vs (if (seq? vs) vs (list vs))]
                        (queue s (unwrap (apply cs (map #(lvar % false) vs))))))
                    empty-s (-> u meta ::when))]
       (first
         (take*
           (fn []
             ((fresh [q]
                (== u w) (== q u)
                (fn [a]
                  (fix-constraints a))
                (reifyg q))
              init-s))))))
  ([u w & ts]
     (if (some #{:when} ts)
       (let [terms (take-while #(not= % :when) ts)
             constraints (last ts)]
         (reduce #(unifier* %1 %2)
           (unifier* (vary-meta u assoc ::when constraints) w)
           terms))
       (apply unifier* (unifier* u w) ts))))

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
     (let [up (vary-meta (prep u) merge (meta u))
           wp (prep w)]
       (unifier* up wp)))
  ([u w & ts]
     (if (some #{:when} ts)
       (let [terms (take-while #(not= % :when) ts)
             constraints (last ts)]
         (reduce #(unifier %1 %2)
           (unifier (vary-meta u assoc ::when constraints) w)
           terms))
       (apply unifier (unifier u w) ts))))

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
    (reduce bind (:a b) gs)))

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

(defn- p->llist
  "Take an lcons pattern and convert it into a llist constructor
   expression."
  ([p vars] (p->llist p vars false))
  ([p vars quoted]
     `(llist
       ~@(doall
           (map #(p->term % vars quoted)
                (remove #(contains? '#{.} %) p))))))

(defn- lvar-sym? [s]
  (and (symbol? s)
       (not= s '.)
       (not (contains? *locals* s))))

(defn update-pvars! [x vars]
  (if (lvar-sym? x)
    (do
      (swap! vars conj x)
      x)
    x))

(defn- p->term
  "Convert a pattern p into a term suitable for unification. Takes an atom
   containing a set for returning any encountered vars which will be declared
   fresh."
  ([p vars] (p->term p vars false))
  ([p vars quoted]
     (cond
       (= p '_) `(lvar)
       (lcons-p? p) (p->llist p vars quoted)
       (coll? p)
       (cond
         ;; support simple expressions
         (seq? p)
         (let [[f s] p]
           (cond
             (= f 'quote)
             (if (and (seq? s) (not quoted))
               (p->term s vars true)
               p) 
             (= f 'clojure.core/unquote)
             (if quoted
               (update-pvars! s vars)
               (throw (Exception. "Invalid use of clojure.core/unquote in pattern.")))
             :else
             (let [ps (map #(p->term % vars quoted) p)]
               (if quoted
                 `(list ~@ps)
                 ps))))
         ;; preserve original collection type
         :else
         (let [ps (map #(p->term % vars quoted) p)]
           (cond
             (instance? clojure.lang.MapEntry p) (into [] ps)
             :else (into (empty p) ps))))
       (symbol? p) (if quoted
                     (list 'quote p)
                     (update-pvars! p vars))
       :else p)))

(defn- fresh-expr? [cs]
  (= (first cs) `fresh))

(defn- ex
  "Takes a list of vars to declare fresh and a term t to be unified
   with relation argument a."
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

(defn- ex*
  "Takes a sequence of pattern/argument pairs, goal expressions and
   a set of seen variables. Returns source code that represents the
   equivalent miniKanren series of unifications."
  [[[p a :as pa] & par] exprs seen]
  (let [vars (atom #{})
        t    (p->term p vars)
        vs   (set/difference @vars seen)
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

(defn everyg
  "A pseudo-relation that takes a coll and ensures that the goal g
   succeeds on every element of the collection."
  [g coll]
  (if (seq coll)
    (all
     (g (first coll))
     (everyg g (next coll)))
    s#))

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

(defne permuteo
  "A relation that will permute xl into the yl. May not
   terminate if xl is not ground."
  [xl yl]
  ([() ()])
  ([[x . xs] _]
     (fresh [ys]
      (permuteo xs ys)
      (rembero x yl ys))))

;; =============================================================================
;; Rel

(defn to-stream [aseq]
  (let [aseq (drop-while nil? aseq)]
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
                             (dissoc m k))
                           m))
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
       (swap! rel-set (fn [s] (reduce disj s tuples)))
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
;; (atom #{}) is cache, waiting streams are PersistentVectors

(deftype SuspendedStream [cache ansv* f]
  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :cache cache
      :ansv* ansv*
      :f f
      not-found))
  ISuspendedStream
  (ready? [this]
    (not= @cache ansv*)))

(defn make-suspended-stream [cache ansv* f]
  (SuspendedStream. cache ansv* f))

(defn suspended-stream? [x]
  (instance? SuspendedStream x))

(defn waiting-stream? [x]
  (vector? x))

(defn waiting-stream-check
  "Take a waiting stream, a success continuation, and a failure continuation.
   If we don't find any ready suspended streams, invoke the failure continuation. 
   If we find a ready suspended stream calculate the remainder of the waiting
   stream. If we've reached the fixpoint just call the thunk of the suspended
   stream, otherwise call mplus on the result of the thunk and the remainder
   of the waiting stream. Pass this result to the success contination."
  [w success-cont failure-cont]
  (loop [w w a []]
    (cond
     (nil? w) (failure-cont)

     (ready? (first w))
     (success-cont
       (fn []
         (let [ss (first w)
               f  (:f ss)
               w  (into a (next w))]
           (if (empty? w)
             (f)
             (mplus (f) (fn [] w))))))

     :else (recur (next w) (conj a (first w))))))

;; -----------------------------------------------------------------------------
;; Extend Substitutions to support tabling

;; CONSIDER: subunify, reify-term-tabled, extending all the necessary types to
;; them

(extend-type Substitutions
  ITabled

  ;; returns a substitution that maps fresh vars to
  ;; new ones. similar to Prolog's copy_term/2. this is to avoid
  ;; prematurely grounding vars.
  (-reify-tabled [this v]
    (let [v (walk this v)]
      (cond
       (lvar? v) (ext-no-check this v (lvar (count (.s this))))
       (coll? v) (-reify-tabled
                   (-reify-tabled this (first v))
                   (next v))
       :else this)))

  ;; returns the term v with all fresh vars replaced with copies.
  ;; this is to avoid prematurely grounding vars.
  (reify-tabled [this v]
    (let [v (walk* this v)]
      (walk* (-reify-tabled empty-s v) v)))

  ;; argv are the actual parameters passed to a goal. cache
  ;; is the cache from the table for reified argv. on initial
  ;; call start is nil and end nil - so internally they will be
  ;; initialized to the contents of the cache & 0
  (reuse [this argv cache start end]
    (let [start (or start @cache)
          end   (or end 0)]
      (letfn [(reuse-loop [ansv*]
                (if (= (count ansv*) end)
                  ;; we've run out of answers terms to reuse in the cache
                  [(make-suspended-stream cache start
                     (fn [] (reuse this argv cache @cache (count start))))]
                  ;; we have answer terms to reuse in the cache
                  (let [ans (first ansv*)]
                    (Choice. (subunify this argv (reify-tabled this ans))
                      (fn [] (reuse-loop (disj ansv* ans)))))))]
        (reuse-loop start))))

  ;; unify an argument with an answer from a cache
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
    (waiting-stream-check this
      ;; success continuation
      (fn [f] (bind f g))
      ;; failure continuation
      (fn []
        (into []
          (map (fn [ss]
                 (make-suspended-stream (:cache ss) (:ansv* ss)
                   (fn [] (bind ((:f ss)) g))))
               this)))))

  IMPlus
  (mplus [this f]
    (waiting-stream-check this
      ;; success continuation
      (fn [fp] (mplus fp f))
      ;; failure continuation
      (fn []
        (let [a-inf (f)]
          (if (waiting-stream? a-inf)
            (into a-inf this)
            (mplus a-inf (fn [] this)))))))

  ITake
  (take* [this]
    (waiting-stream-check this (fn [f] (take* f)) (fn [] ()))))

(defn master
  "Take the argument to the goal and check that we don't
   have an alpha equivalent cached answer term in the cache.
   If it doesn't already exist in the cache add the new
   answer term."
  [argv cache]
  (fn [a]
    (let [rargv (-reify a argv)]
      (when-not (contains? @cache rargv)
        (swap! cache
          (fn [cache]
            (if (contains? cache rargv)
              cache
              (conj cache (reify-tabled a argv)))))
        a))))

;; -----------------------------------------------------------------------------
;; Syntax

;; TODO: consider the concurrency implications much more closely

(defmacro tabled
  "Macro for defining a tabled goal. Prefer ^:tabled with the 
  defne/a/u forms over using this directly."
  [args & grest]
  (let [uuid (symbol (str "tabled-" (UUID/randomUUID)))]
    `(fn ~uuid [~@args]
       (let [argv# ~args]
         (fn [a#]
           (let [key#    (-reify a# argv#)
                 tables# (:ts a#)
                 tables# (if-not (contains? @tables# ~uuid)
                           (swap! tables#
                             (fn [tables#]
                               (if (contains? tables# ~uuid)
                                 tables#
                                 (assoc tables# ~uuid (atom {})))))
                           @tables#)
                 table#  (get tables# ~uuid)]
             (if-not (contains? @table# key#)
               (let [table# (swap! table#
                              (fn [table#]
                                (if (contains? table# key#)
                                  table#
                                  (assoc table# key# (atom #{})))))
                     cache# (get table# key#)]
                 ((fresh []
                    ~@grest
                    (master argv# cache#)) a#))
               (let [cache# (get @table# key#)]
                 (reuse a# argv# cache# nil nil)))))))))

;; =============================================================================
;; cKanren

;; See - Claire Alvis, Dan Friedman, Will Byrd, et al
;; "cKanren - miniKanren with Constraints"
;; http://www.schemeworkshop.org/2011/papers/Alvis2011.pdf
;; http://github.com/calvis/cKanren

(defn get-dom-fd
  [a x]
  (if (lvar? x)
    (get-dom a x ::fd)
    x))

(defn ext-dom-fd
  [a x dom]
  (let [domp (get-dom-fd a x)
        a    (add-dom a x ::fd dom)]
    (if (not= domp dom)
      ((run-constraints* [x] (:cs a) ::fd) a)
      a)))

(defn addcg [c]
  (fn [a]
    (let [a (reduce (fn [a x]
                      (ext-no-check a x (subst-val ::unbound)))
              a (unbound-rands a c))]
      (assoc a :cs (addc (:cs a) a c)))))

(defn updatecg [c]
  (fn [a]
    (assoc a :cs (updatec (:cs a) a c))))

(defn remcg [c]
  (fn [a]
    (assoc a :cs (remc (:cs a) a c))))

(defn runcg [c]
  (fn [a]
    (assoc a :cs (runc (:cs a) c true))))

(defn stopcg [c]
  (fn [a]
    (assoc a :cs (runc (:cs a) c false))))

(defn relevant? [c a]
  (let [id (id c)]
    (and (or ((-> a :cs :cm) id)
             (nil? id))
         (-relevant? c a))))

(defn run-constraint [c]
  (fn [a]
    (if (relevant? c a)
      (if (runnable? c a)
        ((composeg* (runcg c) c (stopcg c)) a)
        a)
      ((remcg c) a))))

;; TODO NOW: try an implementation that allows constraints
;; to run roughly in the order they normaly would. reverse
;; xcs in run-constraints, (into cq (reverse xcs)), cq should
;; be persistent list.

;; TRIED: but causes overflow errors for crypt1, and if we switch to BigInt
;; for crypt1 out of memory errors, needs more investigation

(defn fix-constraints
  "A goal to run the constraints in cq until it is empty. Of
   course running a constraint may grow cq so this function
   finds the fixpoint."
  [a]
  (loop [a a]
    (when a
      (let [cq (:cq a)]
        (if (zero? (count cq))
          (assoc a :cq nil)
          (let [c (first cq)]
            (recur
              ((run-constraint c)
               (-> a
                 (assoc :cq (subvec (:cq a) 1))
                 (assoc :cqs (disj (:cqs a) (id c))))))))))))

(defn run-constraints [xcs]
  (fn [a]
    (let [cq (:cq a)
          a  (reduce (fn [a c]
                       (queue a c))
               (assoc a :cq (or cq [])) xcs)]
     (if cq
       a
       (fix-constraints a)))))

(defn run-constraints* [xs cs ws]
  (if (or (zero? (count cs))
          (nil? (seq xs)))
    s#
    (fn [a]
      (let [xcs (constraints-for cs a (first xs) ws)]
        (if (seq xcs)
          (bind* a
            (run-constraints xcs)
            (run-constraints* (next xs) cs ws))
          (bind a (run-constraints* (next xs) cs ws)))))))

;; TODO: we've hard coded finite domains here

(defn verify-all-bound [a constrained]
  (letfn [(verify-all-bound* [a constrained]
            (when constrained
              (let [x (first constrained)]
                (if (and (lvar? x)
                         (and (lvar? (walk a x))
                              (nil? (get-dom-fd a x))))
                  (throw (Exception. (str "Constrained variable " x " without domain")))
                  (recur a (next constrained))))))]
    (verify-all-bound* a (seq constrained))))

(defn enforceable-constrained [a]
  (let [cs (:cs a)
        km (:km cs)
        cm (:cm cs)
        vs (keys km)]
    (filter (fn [v]
              (some (fn [cid]
                      (when-let [c (get cm cid)]
                        (enforceable? c)))
                    (get km v)))
            vs)))

;; TODO: we've hard coded force-ans here

(declare force-ans)

(defn enforce-constraints [x]
  (all
   (force-ans x)
   (fn [a]
     (let [constrained (enforceable-constrained a)]
       (verify-all-bound a constrained)
       ((onceo (force-ans constrained)) a)))))

(defn reify-constraints [v r a]
  (let [cs  (:cs  a)
        rcs (->> (vals (:cm cs))
                 (filter reifiable?)
                 (map #(reifyc % v r a))
                 (filter #(not (nil? %))))]
    (if (empty? rcs)
      (choice (list v) empty-f)
      (choice (list `(~v :- ~@rcs)) empty-f))))

(defn reifyg [x]
  (all
   (enforce-constraints x)
   (fn [a]
     (let [v (walk* a x)
           r (-reify* empty-s v)]
       (if (zero? (count r))
         (choice (list v) empty-f)
         (let [v (walk* r v)]
           (reify-constraints v r a)))))))


(defn cgoal [c]
  (reify
    clojure.lang.IFn
    (invoke [_ a]
      (if (runnable? c a)
        (when-let [a (c a)]
          (if (relevant? c a)
            ((addcg c) a)
            a))
        ((addcg c) a)))
    IUnwrapConstraint
    (unwrap [_] c)))

;; =============================================================================
;; CLP(FD)

;; NOTE: aliasing FD? for solving problems like zebra - David

(defn singleton-dom? [x]
  (integer? x))

(defmacro let-dom
  [a vars & body]
  (let [get-var-dom (fn [a [v b]]
                      `(~b (let [v# (walk ~a ~v)]
                             (if (lvar? v#)
                               (get-dom-fd ~a v#)
                               v#))))]
   `(let [~@(mapcat (partial get-var-dom a) (partition 2 vars))]
      ~@body)))

(defn resolve-storable-dom
  [a x dom]
  (if (singleton-dom? dom)
    (ext-run-cs (rem-dom a x ::fd) x dom)
    (ext-dom-fd a x dom)))

(defn update-var-dom
  [a x dom]
  (let [domp (get-dom-fd a x)]
    (if domp
      (let [i (intersection dom domp)]
        (when i
          (resolve-storable-dom a x i)))
      (resolve-storable-dom a x dom))))

(defn process-dom
  "If x is a var we update its domain. If it's an integer
   we check that it's a member of the given domain."
  [x dom]
  (fn [a]
    (when dom
      (cond
       (lvar? x) (update-var-dom a x dom)
       (member? dom x) a
       :else nil))))

(declare domfdc)

(defn domfd
  "Assign a var x a domain."
  [x dom]
  (fn [a]
    ((composeg
      (process-dom x dom)
      (if (nil? (get-dom-fd a x))
        (domfdc x)
        identity)) a)))

(defmacro infd
  "Assign vars to domain. The domain must come last."
  [& xs-and-dom]
  (let [xs (butlast xs-and-dom)
        dom (last xs-and-dom)
        domsym (gensym "dom_")]
    `(let [~domsym ~dom]
      (fresh []
        ~@(map (fn [x]
                 `(domfd ~x ~domsym))
               xs)))))

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

(defn sort-by-member-count [a]
  (fn [x y]
    (let-dom a [x dx y dy]
      (< (member-count dx) (member-count dy)))))

(defn sort-by-strategy [v x a]
  (case (-> x meta ::strategy)
    ::ff (seq (sort (sort-by-member-count a) v))
    ;; TODO: throw on non-existant strategies
    v))

;; TODO: handle all Clojure tree types
(extend-protocol IForceAnswerTerm
  nil
  (-force-ans [v x] s#)

  Object
  (-force-ans [v x]
    (if (lvar? x)
      (ext-run-csg x v)
      s#))

  clojure.lang.Sequential
  (-force-ans [v x]
    (letfn [(loop [ys]
              (if ys
                (all
                  (force-ans (first ys))
                  (fn [a]
                    (if-let [ys (sort-by-strategy (next ys) x a)]
                      ((loop ys) a)
                      a)))
                s#))]
      (loop (seq v))))

  clojure.lang.IPersistentMap
  (-force-ans [v x]
    (letfn [(loop [ys]
              (if ys
                (all
                  (force-ans (val (first ys)))
                  (loop (next ys)))
                s#))]
      (loop (seq v))))

  clojure.core.logic.LCons
  (-force-ans [v x]
    (letfn [(loop [ys]
              (all
               (force-ans (lfirst ys))
               (if (lcons? (lnext ys))
                 (loop (lnext ys))
                 s#)))]
      (loop v)))

  FiniteDomain
  (-force-ans [v x]
    ((map-sum (fn [n] (ext-run-csg x n))) (to-vals v)))

  IntervalFD
  (-force-ans [v x]
    ((map-sum (fn [n] (ext-run-csg x n))) (to-vals v)))

  MultiIntervalFD
  (-force-ans [v x]
    ((map-sum (fn [n] (ext-run-csg x n))) (to-vals v))))

(defn force-ans [x]
  (fn [a]
    ((let [v (walk a x)]
       (if (lvar? v)
         (-force-ans (get-dom-fd a x) v)
         (let [x (root-var a x)]
           (if (sequential? v)
             (-force-ans (sort-by-strategy v x a) x)
             (-force-ans v x))))) a)))

(deftype FDConstraint [proc _id _meta]
  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :proc proc
      not-found))
  Object
  (equals [this o]
    (if (instance? FDConstraint o)
      (and (= (rator this) (rator o))
           (= (rands this) (rands o)))
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
  IConstraintOp
  (rator [_] (rator proc))
  (rands [_] (rands proc))
  IRelevant
  (-relevant? [this s]
    (-relevant? proc s))
  IRunnable
  (runnable? [this s]
    (if (instance? clojure.core.logic.IRunnable proc)
      (runnable? proc s)
      (letfn [(has-dom? [x]
                (let [x (walk s x)]
                  (if (lvar? x)
                    (get-dom-fd s x)
                    x)))]
        (every? identity (map has-dom? (rands proc))))))
  IWithConstraintId
  (with-id [this new-id] (FDConstraint. (with-id proc new-id) new-id _meta))
  IConstraintId
  (id [this] _id)
  IConstraintWatchedStores
  (watched-stores [this]
    (if (instance? clojure.core.logic.IConstraintWatchedStores proc)
      (watched-stores proc)
      #{::subst ::fd})))

(defn fdc [proc]
  (FDConstraint. proc nil nil))

(defmethod print-method FDConstraint [x ^Writer writer]
  (let [cid (if-let [cid (id x)]
             (str cid ":")
             "")]
    (.write writer (str "(" cid (rator (:proc x)) " " (apply str (interpose " " (rands (:proc x)))) ")"))))

(defn -domfdc [x]
  (reify
    clojure.lang.IFn
    (invoke [this s]
      (when (member? (get-dom-fd s x) (walk s x))
        (rem-dom s x ::fd)))
    IConstraintOp
    (rator [_] `domfdc)
    (rands [_] [x])
    IRelevant
    (-relevant? [this s]
      (not (nil? (get-dom-fd s x))))
    IRunnable
    (runnable? [this s]
      (not (lvar? (walk s x))))
    IConstraintWatchedStores
    (watched-stores [this] #{::subst})))

(defn domfdc [x]
  (cgoal (fdc (-domfdc x))))

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
    (-relevant? [this s]
      (let-dom s [u du v dv]
        (cond
         (not (singleton-dom? du)) true
         (not (singleton-dom? dv)) true
         :else (not= du dv))))))

(defn =fd
  "A finite domain constraint. u and v must be equal. u and v must
   eventually be given domains if vars."
  [u v]
  (cgoal (fdc (=fdc u v))))

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
         :else (when-let [udiff (difference du dv)]
                 ((process-dom u udiff) s)))))
    IConstraintOp
    (rator [_] `!=fd)
    (rands [_] [u v])
    IRelevant
    (-relevant? [this s]
      (let-dom s [u du v dv]
        (not (and (domain? du) (domain? dv) (disjoint? du dv)))))
    IRunnable
    (runnable? [this s]
      (let-dom s [u du v dv]
        ;; we are runnable if du and dv both have domains
        ;; and at least du or dv has a singleton domain
        (and (domain? du) (domain? dv)
             (or (singleton-dom? du)
                 (singleton-dom? dv))))))) 

(defn !=fd
  "A finite domain constraint. u and v must not be equal. u and v
   must eventually be given domains if vars."
  [u v]
  (cgoal (fdc (!=fdc u v))))

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
    (-relevant? [this s]
      (let-dom s [u du v dv]
       (if (and (domain? du) (domain dv))
         (if (and (singleton-dom? du) (singleton-dom? dv))
           (not (<= du dv))
           (not (interval-< du dv)))
         true)))))

(defn <=fd
  "A finite domain constraint. u must be less than or equal to v.
   u and v must eventually be given domains if vars."
  [u v]
  (cgoal (fdc (<=fdc u v))))

(defn <fd
  "A finite domain constraint. u must be less than v. u and v
   must eventually be given domains if vars."
  [u v]
  (all
   (<=fd u v)
   (!=fd u v)))

(defn >fd
  "A finite domain constraint. u must be greater than v. u and v
   must eventually be given domains if vars."
  [u v]
  (<fd v u))

(defn >=fd
  "A finite domain constraint. u must be greater than or equal to v.
   u and v must eventually be given domains if vars."
  [u v]
  (<=fd v u))

;; NOTE: we could put logic right back in but then we're managing
;; the constraint in the body again which were trying to get
;; away from

(defn +fdc-guard [u v w]
  (fn [s]
    (let-dom s [u du v dv w dw]
      (if (every? singleton-dom? [du dv dw])
        (when (= (+ du dv) dw)
          s)
        s))))

(defn +fdc [u v w]
  (reify 
    clojure.lang.IFn
    (invoke [this s]
      (let-dom s [u du v dv w dw]
        (let [[wmin wmax] (if (domain? dw)
                            (bounds dw)
                            [(+ (lb du) (lb dv)) (+ (ub du) (ub dv))])
              [umin umax] (if (domain? du)
                            (bounds du)
                            [(- (lb dw) (ub dv)) (- (ub dw) (lb dv))])
              [vmin vmax] (if (domain? dv)
                            (bounds dv)
                            [(- (lb dw) (ub du)) (- (ub dw) (lb du))])]
          ((composeg*
            (process-dom w (interval (+ umin vmin) (+ umax vmax)))
            (process-dom u (interval (- wmin vmax) (- wmax vmin)))
            (process-dom v (interval (- wmin umax) (- wmax umin)))
            (+fdc-guard u v w))
           s))))
    IConstraintOp
    (rator [_] `+fd)
    (rands [_] [u v w])
    IRelevant
    (-relevant? [this s]
      (let-dom s [u du v dv w dw]
        (cond
         (not (singleton-dom? du)) true
         (not (singleton-dom? dv)) true
         (not (singleton-dom? dw)) true
         :else (not= (+ du dv) dw))))
    IRunnable
    (runnable? [this s]
      ;; we want to run even if w doesn't have a domain
      ;; this is to support eqfd
      (let-dom s [u du v dv w dw]
        (cond
          (domain? du) (or (domain? dv) (domain? dw))
          (domain? dv) (or (domain? du) (domain? dw))
          (domain? dw) (or (domain? du) (domain? dv))
          :else false)))))

(defn +fd
  "A finite domain constraint for addition and subtraction.
   u, v & w must eventually be given domains if vars."
  [u v w]
  (cgoal (fdc (+fdc u v w))))

(defn -fd
  [u v w]
  (+fd v w u))

;; TODO NOW: we run into trouble with division this is why
;; simplefd in bench.clj needs map-sum when it should not

(defn *fdc-guard [u v w]
  (fn [s]
    (let-dom s [u du v dv w dw]
      (if (every? singleton-dom? [du dv dw])
        (when (= (* du dv) dw)
          s)
        s))))

(defn *fdc [u v w]
  (letfn [(safe-div [n c a t]
            (if (zero? n)
              c
              (let [q (quot a n)]
                (case t
                  :lower (if (pos? (rem a n))
                           (inc q)
                           q)
                  :upper q))))]
   (reify
     clojure.lang.IFn
     (invoke [this s]
       (let-dom s [u du v dv w dw]
         (let [[wmin wmax] (if (domain? dw)
                             (bounds dw)
                             [(* (lb du) (lb dv)) (* (ub du) (ub dv))])
               [umin umax] (if (domain? du)
                             (bounds du)
                             [(safe-div (ub dv) (lb dw) (lb dw) :lower)
                              (safe-div (lb dv) (lb dw) (ub dw) :upper)])
               [vmin vmax] (if (domain? dv)
                             (bounds dv)
                             [(safe-div (ub du) (lb dw) (lb dw) :lower)
                              (safe-div (lb du) (lb dw) (ub dw) :upper)])
               wi (interval (* umin vmin) (* umax vmax))
               ui (interval (safe-div vmax umin wmin :lower)
                            (safe-div vmin umax wmax :upper))
               vi (interval (safe-div umax vmin wmin :lower)
                            (safe-div umin vmax wmax :upper))]
           ((composeg*
             (process-dom w wi)
             (process-dom u ui)
             (process-dom v vi)
             (*fdc-guard u v w)) s))))
     IConstraintOp
     (rator [_] `*fd)
     (rands [_] [u v w])
     IRelevant
     (-relevant? [this s]
       (let-dom s [u du v dv w dw]
         (cond
          (not (singleton-dom? du)) true
          (not (singleton-dom? dv)) true
          (not (singleton-dom? dw)) true
          :else (not= (* du dv) dw))))
     IRunnable
     (runnable? [this s]
       ;; we want to run even if w doesn't have a domain
       ;; this is to support eqfd
       (let-dom s [u du v dv w dw]
         (cond
          (domain? du) (or (domain? dv) (domain? dw))
          (domain? dv) (or (domain? du) (domain? dw))
          (domain? dw) (or (domain? du) (domain? dv))
          :else false))))))

(defn *fd
  "A finite domain constraint for multiplication and
   thus division. u, v & w must be eventually be given 
   domains if vars."
  [u v w]
  (cgoal (fdc (*fdc u v w))))

(defn quotfd [u v w]
  (*fd v w u))

(defn -distinctfdc
  "The real *individual* distinctfd constraint. x is a var that now is bound to
   a single value. y* were the non-singleton bound vars that existed at the
   construction of the constraint. n* is the set of singleton domain values 
   that existed at the construction of the constraint. We use categorize to 
   determine the current non-singleton bound vars and singleton vlaues. if x
   is in n* or the new singletons we have failed. If not we simply remove 
   the value of x from the remaining non-singleton domains bound to vars."
  ([x y* n*] (-distinctfdc x y* n* nil))
  ([x y* n* id]
     (reify
       clojure.lang.IFn
       (invoke [this s]
         (let [x (walk s x)]
           (when-not (n* x)
             (loop [y* (seq y*) s s]
               (if y*
                 (let [y (first y*)
                       v (or (get-dom-fd s y) (walk s y))
                       s (if-not (lvar? v)
                           (cond
                             (= x v) nil
                             (member? v x) ((process-dom y (difference v x)) s)
                             :else s)
                           s)]
                   (when s
                     (recur (next y*) s)))
                 ((remcg this) s))))))
       IWithConstraintId
       (with-id [this id]
         (-distinctfdc x y* n* id))
       IConstraintId
       (id [this] id)
       IConstraintOp
       (rator [_] `-distinctfd)
       (rands [_] [x])
       IRelevant
       (-relevant? [this s] true) ;; we are relevant until we run
       IRunnable
       (runnable? [this s]
         ;; we can only run if x is is bound to
         ;; a single value
         (singleton-dom? (walk s x)))
       IConstraintWatchedStores
       (watched-stores [this] #{::subst}))))

(defn -distinctfd [x y* n*]
  (cgoal (fdc (-distinctfdc x y* n*))))

(defn list-sorted? [pred ls]
  (if (empty? ls)
    true
    (loop [f (first ls) ls (next ls)]
      (if ls
        (let [s (first ls)]
         (if (pred f s)
           (recur s (next ls))
           false))
        true))))

(defn distinctfdc
  "The real distinctfd constraint. v* can be seq of logic vars and
   values or it can be a logic var itself. This constraint does not 
   run until v* has become ground. When it has become ground we group
   v* into a set of logic vars and a sorted set of known singleton 
   values. We then construct the individual constraint for each var."
  ([v*] (distinctfdc v* nil))
  ([v* id]
     (reify
       clojure.lang.IFn
       (invoke [this s]
         (let [v* (walk s v*)
               {x* true n* false} (group-by lvar? v*)
               n* (sort < n*)]
           (when (list-sorted? < n*)
             (let [x* (into #{} x*)
                   n* (into (sorted-set) n*)]
               (loop [xs (seq x*) s s]
                 (if xs
                   (let [x (first xs)]
                     (when-let [s ((-distinctfd x (disj x* x) n*) s)]
                       (recur (next xs) s)))
                   ((remcg this) s)))))))
       IWithConstraintId
       (with-id [this id]
         (distinctfdc v* id))
       IConstraintId
       (id [this] id)
       IConstraintOp
       (rator [_] `distinctfd)
       (rands [_] [v*])
       IRelevant
       (-relevant? [this s]
         true)
       IRunnable
       (runnable? [this s]
         (let [v* (walk s v*)]
           (not (lvar? v*))))
       IConstraintWatchedStores
       (watched-stores [this] #{::subst}))))

(defn distinctfd
  "A finite domain constraint that will guarantee that 
   all vars that occur in v* will be unified with unique 
   values. v* need not be ground. Any vars in v* should
   eventually be given a domain."
  [v*]
  (cgoal (fdc (distinctfdc v*))))

(defne bounded-listo
  "Ensure that the list l never grows beyond bound n.
   n must have been assigned a domain."
  [l n]
  ([() _] (<=fd 0 n))
  ([[h . t] n]
     (fresh [m]
       (infd m (interval 0 Integer/MAX_VALUE))
       (+fd m 1 n)
       (bounded-listo t m))))

(defn distribute [v* strategy]
  (fn [a]
    (add-attr a v* ::strategy ::ff)))

;; -----------------------------------------------------------------------------
;; FD Equation Sugar

(def binops->fd
  '{+  clojure.core.logic/+fd
    -  clojure.core.logic/-fd
    *  clojure.core.logic/*fd
    /  clojure.core.logic/quotfd
    =  clojure.core.logic/==
    != clojure.core.logic/!=fd
    <= clojure.core.logic/<=fd
    <  clojure.core.logic/<fd
    >= clojure.core.logic/>=fd
    >  clojure.core.logic/>fd})

(def binops (set (keys binops->fd)))

(defn expand [form]
  (if (seq? form)
    (let [[op & args] form]
     (if (and (binops op) (> (count args) 2))
       (list op (expand (first args))
             (expand (cons op (rest args))))
       (cons op (map expand args))))
    form))

(defn eqfd*
  ([form vars] (eqfd* form vars nil))
  ([form vars out]
     (if (seq? form)
       (let [[op r1 r2] form
             [outl outlv?] (if (seq? r1)
                             (let [s (gensym)]
                               (swap! vars conj s)
                               [s true])
                             [r1 false])
             [outr outrv?] (if (seq? r2)
                             (let [s (gensym)]
                               (swap! vars conj s)
                               [s true])
                             [r2 false])
             op (binops->fd op)]
         (cons (if out
                 (list op outl outr out)
                 (list op outl outr))
               (concat (when (seq? r1)
                         (eqfd* r1 vars (when outlv? outl)))
                       (when (seq? r2)
                         (eqfd* r2 vars (when outrv? outr))))))
       form)))

(defn ->fd [vars exprs]
  `(fresh [~@vars]
     ~@(reverse exprs)))

(defn eqfd-form [form]
  (let [vars (atom [])
        exprs (eqfd* (expand form) vars)]
    (->fd @vars exprs)))

(defmacro eqfd [& forms]
  `(all
    ~@(map eqfd-form forms)))

;; =============================================================================
;; CLP(Tree)

(defn disunify
  ([s u v] (disunify s u v {:prefixc {}}))
  ([s u v cs]
     (if (identical? u v)
       cs
       (let [u (walk s u)
             v (walk s v)]
         (if (identical? u v)
           cs
           (if (and (not (lvar? u)) (lvar? v))
             (disunify-terms v u s cs)
             (disunify-terms u v s cs)))))))

(extend-protocol IDisunifyTerms
  nil
  (disunify-terms [u v s cs]
    (if-not (nil? v) nil cs))

  Object
  (disunify-terms [u v s cs]
    (if-not (= u v) nil cs))

  clojure.core.logic.LVar
  (disunify-terms [u v s {pc :prefixc :as cs}]
    (assoc cs :prefixc (assoc pc u v)))

  clojure.lang.Sequential
  (disunify-terms [u v s cs]
    (if (sequential? v)
      (loop [u (seq u) v (seq v) cs cs]
        (if u
          (if v
            (let [uv (first u)
                  vv (first v)
                  cs (disunify s uv vv cs)]
              (if cs
                (recur (next u) (next v) cs)
                nil))
            nil)
          (if (nil? v)
            cs
            nil)))
      nil))

  clojure.lang.IPersistentMap
  (disunify-terms [u v s cs]
    (if (= (count u) (count v))
      (loop [ks (seq (keys u)) cs cs]
        (if ks
          (let [kf (first ks)
                vf (get v kf ::not-found)]
            (when-not (= vf ::not-found)
              (if-let [cs (disunify s (get u kf) vf cs)]
                (recur (next ks) cs)
                nil))) 
          cs))
      nil)))

#_(defn prefix-subsumes? [p pp]
  (let [s (-> p meta :s)
        sp (reduce (fn [s [lhs rhs]]
                     (unify s lhs rhs))
                   s pp)]
    (when sp
      (identical? s sp))))

(defn recover-vars [p]
  (loop [p (seq p) r #{}]
    (if p
      (let [[u v] (first p)]
        (if (lvar? v)
          (recur (next p) (conj r u v))
          (recur (next p) (conj r u))))
      r)))

(declare normalize-store)

(defn !=c
  ([p] (!=c p nil))
  ([p id]
     (reify
       clojure.lang.IFn
       (invoke [this a]
         (let [p (loop [sp (seq p) p p]
                   (if sp
                     (let [[x v] (first sp)
                           ;; TODO: this seems expensive to walk* both sides
                           ;; and run an equality test there must be a better
                           ;; way - David
                           xv (walk* a x)
                           vv (walk* a v)]
                       (cond
                         (= xv vv) (recur (next sp) (dissoc p x))
                         (and (not (lvar? xv)) (not (lvar? vv)) (not= xv vv)) nil
                         :else (recur (next sp) p)))
                     p))]
           (if p
             (when-not (empty? p)
               #_((normalize-store (with-prefix this p)) a)
               ((updatecg (with-prefix this p)) a))
             ((remcg this) a))))
       ITreeConstraint
       (tree-constraint? [_] true)
       IWithConstraintId
       (with-id [_ id] (!=c p id))
       IConstraintId
       (id [_] id)
       IPrefix
       (prefix [_] p)
       IWithPrefix
       (with-prefix [_ p] (!=c p id))
       IEnforceableConstraint
       (enforceable? [_] false)
       IReifiableConstraint
       (reifyc [this v r a]
         (let [p* (walk* r (map (fn [[lhs rhs]] `(~lhs ~rhs)) p))]
           (if (empty? p*)
             '()
             (let [p* (filter (fn [[lhs rhs]]
                                (not (or (var? lhs)
                                         (var? rhs))))
                              p*)]
               `(~'!= ~@(first p*))))))
       IConstraintOp
       (rator [_] `!=)
       (rands [_] (seq (recover-vars p)))
       IRunnable
       (runnable? [this s]
         (some #(not= (walk s %) %) (recover-vars p)))
       IRelevant
       (-relevant? [this s]
         (not (empty? p)))
       IRelevantVar
       (-relevant-var? [this x]
         ((recover-vars p) x))
       IConstraintWatchedStores
       (watched-stores [this] #{::subst}))))

#_(defn normalize-store [c]
  (fn [a]
    (let [p (prefix c)
          cid (id c)
          cs (:cs a)
          cids (->> (seq (recover-vars p))
                    (mapcat (:km cs))
                    (remove nil?)
                    (into #{}))
          neqcs (->> (seq cids)
                     (map (:cm cs))
                     (filter tree-constraint?)
                     (remove #(= (id %) cid)))]
      (loop [a a neqcs (seq neqcs)]
        (if neqcs
          (let [oc (first neqcs)
                pp (prefix oc)]
            (cond
             (prefix-subsumes? pp p) ((remcg c) a)
             (prefix-subsumes? p pp) (recur (assoc a :cs (remc cs a oc)) (next neqcs))
             :else (recur a (next neqcs))))
          ((updatecg c) a))))))

(defn !=
  "Disequality constraint. Ensures that u and v will never
   unify. u and v can be complex terms."
  [u v]
  (fn [a]
    (let [cs (disunify a u v)]
      (if-not (nil? cs)
        (let [p (:prefixc cs)]
          (when-not (empty? p)
            ((cgoal (!=c p)) a)))
        a))))

(defne distincto
  "A relation which guarantees no element of l will unify
   with another element of l."
  [l]
  ([()])
  ([[h]])
  ([[h0 h1 . t]]
     (!= h0 h1)
     (distincto (lcons h0 t))
     (distincto (lcons h1 t))))

(defne rembero
  "A relation between l and o where is removed from
   l exactly one time."
  [x l o]
  ([_ [x . xs] xs])
  ([_ [y . ys] [y . zs]]
     (!= y x)
     (rembero x ys zs)))

;; =============================================================================
;; Partial Maps

(defprotocol IUnifyWithPMap
  (unify-with-pmap [pmap u s]))

(defn unify-with-pmap* [u v s]
  (loop [ks (keys u) s s]
    (if (seq ks)
      (let [kf (first ks)
            vf (get v kf ::not-found)
            uf (get u kf)]
        (if (= vf ::not-found)
          (if (= uf ::not-found)
            (recur (next ks) s)
            nil)
          (if (= uf ::not-found)
            nil
            (if-let [s (unify s uf vf)]
              (recur (next ks) s)
              nil))))
      s)))

(declare partial-map?)

(defrecord PMap []
  INonStorable

  IUnifyTerms
  (unify-terms [u v s]
    (if (map? v)
      (unify-with-pmap* u v s)
      nil))

  IUnifyWithRecord
  (unify-with-record [u v s]
    (if (map? v)
      (unify-with-pmap* u v s)
      nil))

  IUninitialized
  (-uninitialized [_] (PMap.))

  IWalkTerm
  (walk-term [v f]
    (walk-record-term v f)))

(defn partial-map
  "Given map m, returns partial map that unifies with maps even if it
   doesn't share all of the keys of that map."
  [m]
  (map->PMap m))

(defn partial-map? [x]
  (instance? PMap x))

(defn -featurec
  ([x fs] (-featurec x (partial-map fs) nil))
  ([x fs _id]
     (reify
       clojure.lang.IFn
       (invoke [this a]
         ((composeg
           (== fs x)
           (remcg this)) a))
       IConstraintId
       (id [this] _id)
       IWithConstraintId
       (with-id [this _id]
         (-featurec x fs _id))
       IConstraintOp
       (rator [_] `featurec)
       (rands [_] [x])
       IReifiableConstraint
       (reifyc [_ v r a]
         (let [fs (into {} fs)
               r  (-reify* r (walk* a fs))]
           `(featurec ~(walk* r x) ~(walk* r fs))))
       IRelevant
       (-relevant? [_ a] true)
       IRunnable
       (runnable? [_ a]
         (not (lvar? (walk a x))))
       IConstraintWatchedStores
       (watched-stores [this] #{::subst}))))

(defn featurec
  "Ensure that a map contains at least the key-value pairs
  in the map fs. fs must be partially instantiated - that is, 
  it may contain values which are logic variables to support 
  feature extraction."
  [x fs]
  (cgoal (-featurec x fs)))

;; =============================================================================
;; defc

(defn ground-term? [x s]
  (letfn [(-ground-term? [x s]
            (let [x (walk s x)]
              (if (lvar? x)
                (throw fk)
                (walk-term x
                  (fn [x]
                    (let [x (walk s x)]
                      (cond
                        (lvar? x) (throw fk)
                        (coll? x) (-ground-term? x s)
                        :else x)))))))]
    (try
      (-ground-term? x s)
      true
      (catch Exception e
        false))))

;; consider ^:partial type hint for arguments
;; these argument only need to be partially instantiated

(defmacro defc [name args & body]
  (let [-name (symbol (str "-" name))]
   `(let [~-name (fn ~-name
                   (~args (~-name ~@args nil))
                   ([~@args id#]
                    (reify
                      ~'clojure.lang.IFn
                      (~'invoke [this# a#]
                        (let [[~@args :as args#] (map #(walk* a# %) ~args)
                              test# (do ~@body)]
                          (when test#
                            ((remcg this#) a#))))
                      clojure.core.logic/IConstraintId
                      (~'id [_#] id#)
                      clojure.core.logic/IWithConstraintId
                      (~'with-id [_# id#] (~-name ~@args id#))
                      clojure.core.logic/IConstraintOp
                      (~'rator [_#] '~name)
                      (~'rands [_#] (filter lvar? (flatten ~args)))
                      clojure.core.logic/IReifiableConstraint
                      (~'reifyc [_# _# r# a#]
                        (list '~name (map #(-reify r# %) ~args)))
                      clojure.core.logic/IRelevant
                      (~'-relevant? [_# s#] true)
                      clojure.core.logic/IRunnable
                      (~'runnable? [_# s#]
                        (ground-term? ~args s#))
                      clojure.core.logic/IConstraintWatchedStores
                      (~'watched-stores [_#] #{::subst}))))]
      (defn ~name ~args
        (cgoal (~-name ~@args))))))

;; =============================================================================
;; Predicate Constraint

(defn -predc
  ([x p] (-predc x p p nil))
  ([x p pform] (-predc x p pform nil))
  ([x p pform _id]
     (reify
       Object
       (toString [_]
         (str pform))
       clojure.lang.IFn
       (invoke [this a]
         (let [x (walk a x)]
           (when (p x)
             ((remcg this) a))))
       IConstraintId
       (id [this] _id)
       IWithConstraintId
       (with-id [this _id]
         (-predc x p pform _id))
       IConstraintOp
       (rator [_] (if (seq? pform)
                    `(predc ~pform)
                    `predc))
       (rands [_] [x])
       IReifiableConstraint
       (reifyc [_ v r a]
         pform)
       IRelevant
       (-relevant? [_ a] true)
       IRunnable
       (runnable? [_ a]
         (not (lvar? (walk a x))))
       IConstraintWatchedStores
       (watched-stores [this] #{::subst}))))

(defn predc
  ([x p] (predc x p p))
  ([x p pform]
     (cgoal (-predc x p pform))))

;; =============================================================================
;; Deep Constraint

(defprotocol IConstrainTree
  (-constrain-tree [t fc s]))

(declare treec)

(extend-protocol IConstrainTree
  clojure.core.logic.LCons
  (-constrain-tree [t fc s]
    (loop [t t s s]
      (if (lvar? t)
        (fc t s)
        (when-let [s (fc (lfirst t) s)]
          (recur (lnext t) s)))))
  
  clojure.lang.Sequential
  (-constrain-tree [t fc s]
    (loop [t (seq t) s s]
      (if t
        (when-let [s (fc (first t) s)]
          (recur (next t) s))
        s)))

  clojure.lang.IPersistentMap
  (-constrain-tree [t fc s]
    (loop [t (seq t) s s]
      (if t
        (let [[_ v] (first t)
              s (fc v s)]
          (when s
            (recur (next t) s)))
        s))))

(defn constrain-tree [t fc]
  (fn [a]
    (-constrain-tree t fc a)))

(defn -fixc
  ([x f reifier] (-fixc x f reifier nil))
  ([x f reifier _id]
     (reify
       clojure.lang.IFn
       (invoke [this a]
         (let [x (walk a x)]
           ((composeg (f x a reifier) (remcg this)) a)))
       IConstraintId
       (id [this] _id)
       IWithConstraintId
       (with-id [this _id]
         (-fixc x f reifier _id))
       IConstraintOp
       (rator [_] `fixc)
       (rands [_] [x])
       IReifiableConstraint
       (reifyc [c v r a]
         (if (fn? reifier)
           (reifier c x v r a)
           (let [x (walk* r x)]
             `(fixc ~x ~reifier))))
       IRelevant
       (-relevant? [_ a] true)
       IRunnable
       (runnable? [_ a]
         (not (lvar? (walk a x))))
       IConstraintWatchedStores
       (watched-stores [this] #{::subst}))))

(defn fixc [x f reifier]
  (cgoal (-fixc x f reifier)))

(defn treec [x fc reifier]
  (fixc x
    (fn loop [t a reifier]
      (if (tree-term? t)
        (constrain-tree t
          (fn [t a] ((fixc t loop reifier) a)))
        (fc t)))
    reifier))
