(ns clojure.core.logic.fd
  (:refer-clojure :exclude [== < > <= >= + - * quot distinct])
  (:use [clojure.core.logic.protocols]
        [clojure.core.logic :exclude [get-dom == != !=c] :as l])
  (:require [clojure.set :as set]
            [clojure.string :as string])
  (:import [java.io Writer]
           [java.util UUID]
           [clojure.core.logic.protocols IEnforceableConstraint]))

(alias 'core 'clojure.core)

;; -----------------------------------------------------------------------------
;; Finite domain protocol types

(defprotocol IInterval
  (-lb [this])
  (-ub [this]))

(defprotocol IIntervals
  (-intervals [this]))

(defprotocol ISortedDomain
  (-drop-one [this])
  (-drop-before [this n])
  (-keep-before [this n]))

(defprotocol ISet
  (-member? [this n])
  (-disjoint? [this that])
  (-intersection [this that])
  (-difference [this that]))

(declare domain sorted-set->domain
         difference* intersection* disjoint?*
         unify-with-domain* finite-domain?
         interval multi-interval)

(defn bounds [i]
  (pair (-lb i) (-ub i)))

(defn interval-< [i j]
  (core/< (-ub i) (-lb j)))

(defn interval-<= [i j]
  (core/<= (-ub i) (-lb j)))

(defn interval-> [i j]
  (core/> (-lb i) (-ub j)))

(defn interval->= [i j]
  (core/>= (-lb i) (-ub j)))

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
      (if (= (-member-count this) (-member-count that))
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
  (-member-count [this] (count s))

  IInterval
  (-lb [_] min)
  (-ub [_] max)

  ISortedDomain
  (-drop-one [_]
    (let [s (disj s min)
          c (count s)]
      (cond
       (= c 1) (first s)
       (core/> c 1) (FiniteDomain. s (first s) max)
       :else nil)))

  (-drop-before [_ n]
    (apply domain (drop-while #(core/< % n) s)))

  (-keep-before [this n]
    (apply domain (take-while #(core/< % n) s)))

  ISet
  (-member? [this n]
    (if (s n) true false))

  (-disjoint? [this that]
    (cond
     (integer? that)
       (if (s that) false true)
     (instance? FiniteDomain that)
       (cond
         (core/< max (:min that)) true
         (core/> min (:max that)) true
         :else (empty? (set/intersection s (:s that))))
     :else (disjoint?* this that)))

  (-intersection [this that]
    (cond
     (integer? that)
       (when (-member? this that) that)
     (instance? FiniteDomain that)
       (sorted-set->domain (set/intersection s (:s that)))
     :else
       (intersection* this that)))

  (-difference [this that]
    (cond
     (integer? that)
       (sorted-set->domain (disj s that))
     (instance? FiniteDomain that)
       (sorted-set->domain (set/difference s (:s that)))
     :else
       (difference* this that)))

  IIntervals
  (-intervals [_] (seq s))

  IMergeDomains
  (-merge-doms [this that]
    (-intersection this that)))

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
     (~'-member-count [this#] 1)

     IInterval
     (~'-lb [this#] this#)
     (~'-ub [this#] this#)

     ISortedDomain
     (~'-drop-one [this#]
       nil)
     (~'-drop-before [this# n#]
       (when (clojure.core/>= this# n#)
         this#))
     (~'-keep-before [this# n#]
       (when (clojure.core/< this# n#)
         this#))

     ISet
     (~'-member? [this# that#]
       (if (integer? that#)
         (= this# that#)
         (-member? that# this#)))
     (~'-disjoint? [this# that#]
       (if (integer? that#)
         (not= this# that#)
         (-disjoint? that# this#)))
     (~'-intersection [this# that#]
       (cond
        (integer? that#) (when (= this# that#)
                           this#)
        (interval? that#) (-intersection that# this#)
        :else (intersection* this# that#)))
     (~'-difference [this# that#]
       (cond
        (integer? that#) (if (= this# that#)
                           nil
                           this#)
        (interval? that#) (-difference that# this#)
        :else (difference* this# that#)))

     IIntervals
     (~'-intervals [this#]
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
;; lb - lower bound
;; ub - upper bound

(deftype IntervalFD [lb ub]
  Object
  (equals [_ o]
    (if (instance? IntervalFD o)
      (and (= lb (-lb o))
           (= ub (-ub o)))
      false))

  (toString [this]
    (pr-str this))

  IMemberCount
  (-member-count [this] (inc (core/- ub lb)))

  IInterval
  (-lb [_] lb)
  (-ub [_] ub)

  ISortedDomain
  (-drop-one [_]
    (let [nlb (inc lb)]
      (when (core/<= nlb ub)
        (interval nlb ub))))

  (-drop-before [this n]
    (cond
     (= n ub) n
     (core/< n lb) this
     (core/> n ub) nil
     :else (interval n ub)))

  (-keep-before [this n]
    (cond
     (core/<= n lb) nil
     (core/> n ub) this
     :else (interval lb (dec n))))

  ISet
  (-member? [this n]
    (and (core/>= n lb) (core/<= n ub)))

  (-disjoint? [this that]
    (cond
     (integer? that)
     (not (-member? this that))

     (interval? that)
     (let [i this
           j that
           [imin imax] (bounds i)
           [jmin jmax] (bounds j)]
       (or (core/> imin jmax)
           (core/< imax jmin)))

     :else (disjoint?* this that)))

  (-intersection [this that]
    (cond
     (integer? that)
     (if (-member? this that)
       that
       nil)

     (interval? that)
     (let [i this j that
           imin (-lb i) imax (-ub i)
           jmin (-lb j) jmax (-ub j)]
       (cond
        (core/< imax jmin) nil
        (core/< jmax imin) nil
        (and (core/<= imin jmin)
             (core/>= imax jmax)) j
        (and (core/<= jmin imin)
             (core/>= jmax imax)) i
        (and (core/<= imin jmin)
             (core/<= imax jmax)) (interval jmin imax)
        (and (core/<= jmin imin)
             (core/<= jmax imax)) (interval imin jmax)
        :else (throw (Error. (str "Interval intersection not defined " i " " j)))))

     :else (intersection* this that)))

  (-difference [this that]
    (cond
     (integer? that)
     (cond
      (= lb that) (interval (inc lb) ub)
      (= ub that) (interval lb (dec ub))
      :else (if (-member? this that)
              (multi-interval (interval lb (dec that))
                              (interval (inc that) ub))
              this))
     
     (interval? that)
     (let [i this j that
           imin (-lb i) imax (-ub i)
           jmin (-lb j) jmax (-ub j)]
       (cond
        (core/> jmin imax) i
        (and (core/<= jmin imin)
             (core/>= jmax imax)) nil
        (and (core/< imin jmin)
             (core/> imax jmax)) (multi-interval (interval imin (dec jmin))
             (interval (inc jmax) imax))
        (and (core/< imin jmin)
             (core/<= jmin imax)) (interval imin (dec jmin))
        (and (core/> imax jmax)
             (core/<= jmin imin)) (interval (inc jmax) imax)
        :else (throw (Error. (str "Interval difference not defined " i " " j)))))
     
     :else (difference* this that)))

  IIntervals
  (-intervals [this]
    (list this))

  IMergeDomains
  (-merge-doms [this that]
    (-intersection this that)))

(defn interval? [x]
  (instance? IntervalFD x))

(defmethod print-method IntervalFD [x ^Writer writer]
  (.write writer (str "<interval:" (-lb x) ".." (-ub x) ">")))

(defn interval
  "Construct an interval for an assignment to a var. intervals may
   be more efficient that the domain type when the range of possiblities
   is large."
  ([ub] (IntervalFD. 0 ub))
  ([lb ub]
     (if (zero? (core/- ub lb))
       ub
       (IntervalFD. lb ub))))

(defn intersection* [is js]
  (loop [is (seq (-intervals is)) js (seq (-intervals js)) r []]
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
            (core/<= imin jmin)
            (cond
             (core/< imax jmax)
             (recur (next is)
                    (cons (interval (inc imax) jmax) (next js))
                    (conj r (interval jmin imax)))
             (core/> imax jmax)
             (recur (cons (interval (inc jmax) imax) (next is))
                    (next js)
                    (conj r j))
             :else
             (recur (next is) (next js)
                    (conj r (interval jmin jmax))))
            (core/> imin jmin)
            (cond
             (core/> imax jmax)
             (recur (cons (interval (inc jmax) imax) (next is))
                    (next js)
                    (conj r (interval imin jmax)))
             (core/< imax jmax)
             (recur is (cons (interval (inc imax) jmax) (next js))
                    (conj r i))
             :else
             (recur (next is) (next js)
                    (conj r (interval imin imax))))))))
      (apply multi-interval r))))

(defn difference* [is js]
    (loop [is (seq (-intervals is)) js (seq (-intervals js)) r []]
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
                (core/< imin jmin)
                (cond
                 (core/< jmax imax)
                 (recur (cons (interval (inc jmax) imax) (next is))
                        (next js)
                        (conj r (interval imin (dec jmin))))
                 (core/> jmax imax)
                 (recur (next is)
                        (cons (interval (inc imax) jmax) (next js))
                        (conj r (interval imin (dec jmin))))
                 :else
                 (recur (next is) (next js)
                        (conj r (interval imin (dec jmin)))))
                (core/>= imin jmin)
                (cond
                 (core/< imax jmax)
                 (recur (next is)
                        (cons (interval (inc imax) jmax) (next js))
                        r)
                 (core/> imax jmax)
                 (recur (cons (interval (inc jmax) imax) (next is))
                        (next js)
                        r)
                 :else (recur (next is) (next js)
                              r))))))
          (apply multi-interval (into r is)))
        (apply multi-interval r))))

(defn disjoint?* [is js]
  (if (-disjoint? (interval (-lb is) (-ub is))
                 (interval (-lb js) (-ub js)))
    true
    (let [d0 (-intervals is)
          d1 (-intervals js)]
      (loop [d0 d0 d1 d1]
        (if (or (nil? d0) (nil? d1))
          true
          (let [i (first d0)
                j (first d1)]
            (cond
              (interval-< i j) (recur (next d0) d1)
              (interval-> i j) (recur d0 (next d1))
              (-disjoint? i j)  (recur (next d0) d1)
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
                js (normalize-intervals (-intervals j))]
            (= is js))
          false))
      false))

  IMemberCount
  (-member-count [this]
    ;; NOTE: ugly hack around http://dev.clojure.org/jira/browse/CLJ-1202 - David
    (reduce core/+ 0 (map #(-member-count %) is)))

  IInterval
  (-lb [_] min)
  (-ub [_] max)

  ISortedDomain
  (-drop-one [_]
    (let [i (first is)]
      (if (singleton-dom? i)
        (let [nis (rest is)]
          (MultiIntervalFD. (-lb (first nis)) max nis))
        (let [ni (-drop-one i)]
          (MultiIntervalFD. (-lb ni) max (cons ni (rest is)))))))

  (-drop-before [_ n]
    (let [is (seq is)]
      (loop [is is r []]
        (if is
          (let [i (-drop-before (first is) n)]
            (if i
              (recur (next is) (conj r i))
              (recur (next is) r)))
          (when (pos? (count r))
            (apply multi-interval r))))))

  (-keep-before [_ n]
    (let [is (seq is)]
      (loop [is is r []]
        (if is
          (let [i (-keep-before (first is) n)]
            (if i
              (recur (next is) (conj r i))
              (recur (next is) r)))
          (when (pos? (count r))
            (apply multi-interval r))))))

  ISet
  (-member? [this n]
    (if (some #(-member? % n) is)
      true
      false))
  (-disjoint? [this that]
    (disjoint?* this that))
  (-intersection [this that]
    (intersection* this that))
  (-difference [this that]
    (difference* this that))

  IIntervals
  (-intervals [this]
    (seq is))

  IMergeDomains
  (-merge-doms [this that]
    (-intersection this that)))

;; union where possible
(defn normalize-intervals [is]
  (reduce (fn [r i]
            (if (zero? (count r))
              (conj r i)
              (let [j (peek r)
                    jmax (-ub j)
                    imin (-lb i)]
                (if (core/<= (dec imin) jmax)
                  (conj (pop r) (interval (-lb j) (-ub i)))
                  (conj r i)))))
          [] is))

(defn multi-interval
  ([] nil)
  ([i0] i0)
  ([i0 i1]
     (let [is [i0 i1]]
       (MultiIntervalFD. (reduce min (map -lb is)) (reduce max (map -ub is)) is)))
  ([i0 i1 & ir]
     (let [is (into [] (concat (list i0 i1) ir))]
       (MultiIntervalFD. (reduce min (map -lb is)) (reduce max (map -ub is)) is))))

(defmethod print-method MultiIntervalFD [x ^Writer writer]
  (.write writer (str "<intervals:" (apply pr-str (:is x)) ">")))

;; =============================================================================
;; CLP(FD)

;; NOTE: aliasing FD? for solving problems like zebra - David

(defn get-dom
  [a x]
  (if (lvar? x)
    (l/get-dom a x ::l/fd)
    x))

(defn ext-dom-fd
  [a x dom]
  (let [domp (get-dom a x)
        a    (add-dom a x ::l/fd dom)]
    (if (not= domp dom)
      ((run-constraints* [x] (:cs a) ::l/fd) a)
      a)))

(defn singleton-dom? [x]
  (integer? x))

(defn resolve-storable-dom
  [a x dom]
  (if (singleton-dom? dom)
    (let [xv (walk a x)]
      (if (lvar? xv)
        (ext-run-cs (rem-dom a x ::l/fd) x dom)
        a))
    (ext-dom-fd a x dom)))

(defn update-var-dom
  [a x dom]
  (let [domp (get-dom a x)]
    (if domp
      (let [i (-intersection dom domp)]
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
       (-member? dom x) a
       :else nil))))

(declare domc)

(defn dom
  "Assign a var x a domain."
  [x dom]
  (fn [a]
    ((composeg
      (process-dom x dom)
      (if (and (nil? (get-dom a x))
               (not (singleton-dom? dom)))
        (domc x)
        identity)) a)))

(defmacro in
  "Assign vars to domain. The domain must come last."
  [& xs-and-dom]
  (let [xs (butlast xs-and-dom)
        dom (last xs-and-dom)
        domsym (gensym "dom_")]
    `(let [~domsym ~dom]
      (fresh []
        ~@(map (fn [x]
                 `(dom ~x ~domsym))
               xs)))))

(defn map-sum [f]
  (fn loop [ls]
    (if (empty? ls)
      (fn [a] nil)
      (conde
        [(f (first ls))]
        [(loop (rest ls))]))))

(defn to-vals [dom]
  (letfn [(to-vals* [is]
            (when is
              (let [i (first is)]
                (lazy-seq
                 (cons (-lb i)
                       (if-let [ni (-drop-one i)]
                         (to-vals* (cons ni (next is)))
                         (to-vals* (next is))))))))]
    (to-vals* (seq (-intervals dom)))))

(extend-protocol IForceAnswerTerm
  FiniteDomain
  (-force-ans [v x]
    ((map-sum (fn [n] (ext-run-csg x n))) (to-vals v)))

  IntervalFD
  (-force-ans [v x]
    ((map-sum (fn [n] (ext-run-csg x n))) (to-vals v)))

  MultiIntervalFD
  (-force-ans [v x]
    ((map-sum (fn [n] (ext-run-csg x n))) (to-vals v))))

(defn -domc [x]
  (reify
    IEnforceableConstraint
    IConstraintStep
    (-step [this s]
      (let [xv (walk s x)
            xd (-> (root-val s x) :doms ::l/fd)]
        (reify
          clojure.lang.IFn
          (invoke [_ s]
            (if xd
              (when (-member? xd xv)
                (rem-dom s x ::l/fd))
              s))
          IEntailed
          (-entailed? [_]
            (nil? xd))
          IRunnable
          (-runnable? [_]
            (not (lvar? xv))))))
    IConstraintOp
    (-rator [_] `domc)
    (-rands [_] [x])
    IConstraintWatchedStores
    (-watched-stores [this] #{::l/subst})))

(defn domc [x]
  (cgoal (-domc x)))

(defn ==c [u v]
  (reify
    IEnforceableConstraint
    IConstraintStep
    (-step [this s]
      (let-dom s [u du v dv]
        (reify
          clojure.lang.IFn
          (invoke [_ s]
            (let [i (-intersection du dv)]
              ((composeg
                 (process-dom u i)
                 (process-dom v i)) s)))
          IEntailed
          (-entailed? [_]
            (and (singleton-dom? du)
                 (singleton-dom? dv)
                 (= du dv)))
          IRunnable
          (-runnable? [_]
            (and du dv)))))
    IConstraintOp
    (-rator [_] `==)
    (-rands [_] [u v])
    IConstraintWatchedStores
    (-watched-stores [this]
      #{::l/subst ::l/fd})))

(defn ==
  "A finite domain constraint. u and v must be equal. u and v must
   eventually be given domains if vars."
  [u v]
  (cgoal (==c u v)))

(defn !=c [u v]
  (reify 
    IEnforceableConstraint
    IConstraintStep
    (-step [this s]
      (let-dom s [u du v dv]
        (let [su? (singleton-dom? du)
              sv? (singleton-dom? dv)]
          (reify
            clojure.lang.IFn
            (invoke [_ s]
              (cond
                (and su? sv? (= du dv)) nil
                (-disjoint? du dv) s
                su? (when-let [vdiff (-difference dv du)]
                      ((process-dom v vdiff) s))
                :else (when-let [udiff (-difference du dv)]
                        ((process-dom u udiff) s))))
            IEntailed
            (-entailed? [_]
              (and du dv (-disjoint? du dv)))
            IRunnable
            (-runnable? [_]
              (and du dv (or su? sv?)))))))
    IConstraintOp
    (-rator [_] `!=)
    (-rands [_] [u v])
    IConstraintWatchedStores
    (-watched-stores [this]
      #{::l/subst ::l/fd}))) 

(defn !=
  "A finite domain constraint. u and v must not be equal. u and v
   must eventually be given domains if vars."
  [u v]
  (cgoal (!=c u v)))

(defn <=c [u v]
  (reify 
    IEnforceableConstraint
    IConstraintStep
    (-step [this s]
      (let-dom s [u du v dv]
        (reify
          clojure.lang.IFn
          (invoke [_ s]
            (let [umin (-lb du)
                  vmax (-ub dv)]
              ((composeg*
                 (process-dom u (-keep-before du (inc vmax)))
                 (process-dom v (-drop-before dv umin))) s)))
          IEntailed
          (-entailed? [_]
            (and du dv (interval-<= du dv)))
          IRunnable
          (-runnable? [_]
            (and du dv)))))
    IConstraintOp
    (-rator [_] `<=)
    (-rands [_] [u v])
    IConstraintWatchedStores
    (-watched-stores [this]
      #{::l/subst ::l/fd})))

(defn <=
  "A finite domain constraint. u must be less than or equal to v.
   u and v must eventually be given domains if vars."
  [u v]
  (cgoal (<=c u v)))

(defn <
  "A finite domain constraint. u must be less than v. u and v
   must eventually be given domains if vars."
  [u v]
  (all
   (<= u v)
   (!= u v)))

(defn >
  "A finite domain constraint. u must be greater than v. u and v
   must eventually be given domains if vars."
  [u v]
  (< v u))

(defn >=
  "A finite domain constraint. u must be greater than or equal to v.
   u and v must eventually be given domains if vars."
  [u v]
  (<= v u))

;; NOTE: we could put logic right back in but then we're managing
;; the constraint in the body again which were trying to get
;; away from

(defn +c-guard [u v w]
  (fn [s]
    (let-dom s [u du v dv w dw]
      (if (every? singleton-dom? [du dv dw])
        (when (= (core/+ du dv) dw)
          s)
        s))))

(defn +c [u v w]
  (reify 
    IEnforceableConstraint
    IConstraintStep
    (-step [this s]
      (let-dom s [u du v dv w dw]
        (reify
          clojure.lang.IFn
          (invoke [_ s]
            (let [[wmin wmax] (if dw
                                (bounds dw)
                                [(core/+ (-lb du) (-lb dv)) (core/+ (-ub du) (-ub dv))])
                  [umin umax] (if du
                                (bounds du)
                                [(core/- (-lb dw) (-ub dv)) (core/- (-ub dw) (-lb dv))])
                  [vmin vmax] (if dv
                                (bounds dv)
                                [(core/- (-lb dw) (-ub du)) (core/- (-ub dw) (-lb du))])]
              ((composeg*
                 (process-dom w (interval (core/+ umin vmin) (core/+ umax vmax)))
                 (process-dom u (interval (core/- wmin vmax) (core/- wmax vmin)))
                 (process-dom v (interval (core/- wmin umax) (core/- wmax umin)))
                 (+c-guard u v w))
                s)))
          IEntailed
          (-entailed? [_]
            (and (singleton-dom? du)
                 (singleton-dom? dv)
                 (singleton-dom? dw)
                 (= (core/+ du dv) dw)))
          IRunnable
          (-runnable? [_]
            (cond
              du (or dv dw)
              dv (or du dw)
              dw (or du dv)
              :else false)))))
    IConstraintOp
    (-rator [_] `+)
    (-rands [_] [u v w])
    IConstraintWatchedStores
    (-watched-stores [this]
      #{::l/subst ::l/fd})))

(defn +
  "A finite domain constraint for addition and subtraction.
   u, v & w must eventually be given domains if vars."
  [u v w]
  (cgoal (+c u v w)))

(defn -
  [u v w]
  (+ v w u))

;; TODO NOW: we run into trouble with division this is why
;; simplefd in bench.clj needs map-sum when it should not

(defn *c-guard [u v w]
  (fn [s]
    (let-dom s [u du v dv w dw]
      (if (every? singleton-dom? [du dv dw])
        (when (= (core/* du dv) dw)
          s)
        s))))

(defn *c [u v w]
  (letfn [(safe-div [n c a t]
            (if (zero? n)
              c
              (let [q (core/quot a n)]
                (case t
                  :lower (if (pos? (rem a n))
                           (inc q)
                           q)
                  :upper q))))]
   (reify
     IEnforceableConstraint
     IConstraintStep
     (-step [this s]
       (let-dom s [u du v dv w dw]
         (reify
           clojure.lang.IFn
           (invoke [_ s]
             (let [[wmin wmax] (if dw
                                 (bounds dw)
                                 [(core/* (-lb du) (-lb dv)) (core/* (-ub du) (-ub dv))])
                   [umin umax] (if du
                                 (bounds du)
                                 [(safe-div (-ub dv) (-lb dw) (-lb dw) :lower)
                                  (safe-div (-lb dv) (-lb dw) (-ub dw) :upper)])
                   [vmin vmax] (if dv
                                 (bounds dv)
                                 [(safe-div (-ub du) (-lb dw) (-lb dw) :lower)
                                  (safe-div (-lb du) (-lb dw) (-ub dw) :upper)])
                    wi (interval (core/* umin vmin) (core/* umax vmax))
                    ui (interval (safe-div vmax umin wmin :lower)
                         (safe-div vmin umax wmax :upper))
                    vi (interval (safe-div umax vmin wmin :lower)
                         (safe-div umin vmax wmax :upper))]
               ((composeg*
                  (process-dom w wi)
                  (process-dom u ui)
                  (process-dom v vi)
                  (*c-guard u v w)) s)))
           IEntailed
           (-entailed? [_]
             (and (singleton-dom? du)
                  (singleton-dom? dv)
                  (singleton-dom? dw)
                  (= (core/* du dv) dw)))
           IRunnable
           (-runnable? [_]
             (cond
               du (or dv dw)
               dv (or du dw)
               dw (or du dv)
               :else false)))))
     IConstraintOp
     (-rator [_] `*)
     (-rands [_] [u v w])
     IConstraintWatchedStores
     (-watched-stores [this]
       #{::l/subst ::l/fd}))))

(defn *
  "A finite domain constraint for multiplication and
   thus division. u, v & w must be eventually be given 
   domains if vars."
  [u v w]
  (cgoal (*c u v w)))

(defn quot [u v w]
  (* v w u))

(defn -distinctc
  "The real *individual* distinct constraint. x is a var that now is bound to
   a single value. y* were the non-singleton bound vars that existed at the
   construction of the constraint. n* is the set of singleton domain values 
   that existed at the construction of the constraint. We use categorize to 
   determine the current non-singleton bound vars and singleton vlaues. if x
   is in n* or the new singletons we have failed. If not we simply remove 
   the value of x from the remaining non-singleton domains bound to vars."
  [x y* n*]
  (reify
    IEnforceableConstraint
    IConstraintStep
    (-step [this s]
      (let [x (walk s x)]
        (reify
          clojure.lang.IFn
          (invoke [_ s]
            (when-not (n* x)
              (loop [y* (seq y*) s s]
                (if y*
                  (let [y (first y*)
                        ;; NOTE: we can't just get-dom because get-dom
                        ;; return nil, walk returns the var - David
                        v (or (get-dom s y) (walk s y))
                        s (if-not (lvar? v)
                            (cond
                              (= x v) nil
                              (-member? v x) ((process-dom y (-difference v x)) s)
                              :else s)
                            s)]
                    (when s
                      (recur (next y*) s)))
                  ((remcg this) s)))))
          IRunnable
          (-runnable? [_]
            (singleton-dom? x)))))
    IConstraintOp
    (-rator [_] `-distinct)
    (-rands [_] [x])
    IConstraintWatchedStores
    (-watched-stores [this] #{::l/subst})))

(defn -distinct [x y* n*]
  (cgoal (-distinctc x y* n*)))

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

(defn distinctc
  "The real distinct constraint. v* can be seq of logic vars and
   values or it can be a logic var itself. This constraint does not 
   run until v* has become ground. When it has become ground we group
   v* into a set of logic vars and a sorted set of known singleton 
   values. We then construct the individual constraint for each var."
  [v*]
  (reify
    IEnforceableConstraint
    IConstraintStep
    (-step [this s]
      (let [v* (walk s v*)]
        (reify
          clojure.lang.IFn
          (invoke [_ s]
            (let [{x* true n* false} (group-by lvar? v*)
                  n* (sort core/< n*)]
              (when (list-sorted? core/< n*)
                (let [x* (into #{} x*)
                       n* (into (sorted-set) n*)]
                  (loop [xs (seq x*) s s]
                    (if xs
                      (let [x (first xs)]
                        (when-let [s ((-distinct x (disj x* x) n*) s)]
                          (recur (next xs) s)))
                      ((remcg this) s)))))))
          IRunnable
          (-runnable? [_]
            (not (lvar? v*))))))
    IConstraintOp
    (-rator [_] `distinct)
    (-rands [_] [v*])
    IConstraintWatchedStores
    (-watched-stores [this] #{::l/subst})))

(defn distinct
  "A finite domain constraint that will guarantee that 
   all vars that occur in v* will be unified with unique 
   values. v* need not be ground. Any vars in v* should
   eventually be given a domain."
  [v*]
  (cgoal (distinctc v*)))

(defne bounded-listo
  "Ensure that the list l never grows beyond bound n.
   n must have been assigned a domain."
  [l n]
  ([() _] (<= 0 n))
  ([[h . t] n]
     (fresh [m]
       (in m (interval 0 Integer/MAX_VALUE))
       (+ m 1 n)
       (bounded-listo t m))))

;; -----------------------------------------------------------------------------
;; FD Equation Sugar

(def binops->fd
  '{+  clojure.core.logic.fd/+
    -  clojure.core.logic.fd/-
    *  clojure.core.logic.fd/*
    /  clojure.core.logic.fd/quot
    =  clojure.core.logic.fd/==
    != clojure.core.logic.fd/!=
    <= clojure.core.logic.fd/<=
    <  clojure.core.logic.fd/<
    >= clojure.core.logic.fd/>=
    >  clojure.core.logic.fd/>})

(def binops (set (keys binops->fd)))

(defn expand [form]
  (if (seq? form)
    (let [[op & args] form]
     (if (and (binops op) (core/> (count args) 2))
       (list op (expand (first args))
             (expand (cons op (rest args))))
       (cons op (map expand args))))
    form))

(defn eq*
  ([form vars] (eq* form vars nil))
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
                         (eq* r1 vars (when outlv? outl)))
                       (when (seq? r2)
                         (eq* r2 vars (when outrv? outr))))))
       form)))

(defn ->fd [vars exprs]
  `(fresh [~@vars]
     ~@(reverse exprs)))

(defn eq-form [form]
  (let [vars (atom [])
        exprs (eq* (expand form) vars)]
    (->fd @vars exprs)))

(defmacro eq [& forms]
  `(all
    ~@(map eq-form forms)))
