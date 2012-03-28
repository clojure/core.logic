(ns clojure.core.logic
  (:refer-clojure :exclude [==])
  (:use [clojure.walk :only [postwalk]])
  (:require [clojure.set :as set])
  (:import [java.io Writer]))

(def ^{:dynamic true} *occurs-check* true)
(def ^{:dynamic true} *locals*)

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

(defprotocol Search
  (yield [a] "Returns the yielded result (if any) or nil.")
  (step [a] "Advances the search -- returns the advanced search.")
  (min-yield [a] "Returns a substitution which is an lower bound of all
    substitutions yielded by this search.")
  (restrict [a ss]
    "Restricts the search to substitutions more precise than ss. ss must be
     greater than or equal to (min-yield a ss). Do not call, see narrow.")
  (sizehint [a]))

(deftype Unbound []) ; replace with reify?
(def ^Unbound unbound (Unbound.))

(defprotocol ILVar
  (constraints [this])
  (add-constraint [this c])
  (add-constraints [this ds])
  (remove-constraint [this c])
  (remove-constraints [this]))

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
    (str "(" lhs " . " rhs ")")))

(defn- ^Pair pair [lhs rhs]
  (Pair. lhs rhs))

;; =============================================================================
;; Substitutions

(defprotocol ISubstitutions
  (length [this])
  (occurs-check [this u v])
  (ext [this u v])
  (ext-no-check [this u v])
  (swap [this cu])
  (constrain [this u c])
  (get-var [this v])
  (use-verify [this f])
  (walk [this v])
  (walk-var [this v])
  (walk* [this v])
  (unify [this u v])
  (reify-lvar-name [_])
  (-reify* [this v])
  (-reify [this v])
  (build [this u]))

(declare empty-s)
(declare lvar)
(declare lvar?)
(declare pair)
(declare lcons)
(declare merge-s)

(deftype Substitutions [s l verify cs]
  Object
  (equals [this o]
    (or (identical? this o)
        (and (.. this getClass (isInstance o))
             (= s ^clojure.lang.PersistentHashMap (.s ^Substitutions o)))))

  ISubstitutions
  (length [this] (count s))

  (occurs-check [this u v]
    (let [v (walk this v)]
      (occurs-check-term v u this)))
  
  (ext [this u v]
    (if (and *occurs-check* (occurs-check this u v))
      nil
      (ext-no-check this u v)))

  (ext-no-check [this u v]
    (verify this u v))

  (swap [this cu]
    (if (contains? s cu)
      (let [v (s cu)]
        (Substitutions. (-> s (dissoc cu) (assoc cu v)) l verify cs))
      (Substitutions. (assoc s cu unbound) l verify cs)))

  (constrain [this u c]
    (let [u (walk this u)]
      (swap this (add-constraint u c))))

  (get-var [this v]
    (first (find s v)))

  (use-verify [this f]
    (Substitutions. s l f cs))
  
  (walk [this v]
    (loop [lv v [v vp] (find s v)]
      (cond
       (nil? v) lv
       (identical? vp unbound) v
       (not (lvar? vp)) vp
       :else (recur vp (find s vp)))))
  
  (walk-var [this v]
    (loop [lv v [v vp] (find s v)]
      (cond
       (nil? v) lv
       (identical? vp unbound) v
       (not (lvar? vp)) v
       :else (recur vp (find s vp)))))
  
  (walk* [this v]
    (let [v (walk this v)]
      (walk-term v this)))

  (unify [this u v]
    (if (identical? u v)
      this
      (let [u (walk this u)
            v (walk this v)]
        (if (identical? u v)
          this
          (unify-terms u v this)))))

  (reify-lvar-name [this]
    (symbol (str "_." (count s))))

  (-reify* [this v]
    (let [v (walk this v)]
      (reify-term v this)))

  (-reify [this v]
    (let [v (walk* this v)]
      (walk* (-reify* empty-s v) v)))

  (build [this u]
    (build-term u this))

  Search
  (yield [this] this)
  (step [this] nil)
  (min-yield [this] this)
  (restrict [this ss] (merge-s this ss))
  (sizehint [this] 0))

(defn- ^Substitutions pass-verify [^Substitutions s u v]
  (Substitutions. (assoc (.s s) u v)
                  (conj (.l s) (pair u v))
                  (.verify s)
                  (.cs s)))

(defn- ^Substitutions make-s
  ([m l] (Substitutions. m l pass-verify nil))
  ([m l f] (Substitutions. m l f nil))
  ([m l f cs] (Substitutions. m l f cs)))

(def ^Substitutions empty-s (make-s {} '()))

(defn ^Substitutions to-s [v]
  (let [s (reduce (fn [m [k v]] (assoc m k v)) {} v)
        l (reduce (fn [l [k v]] (conj l (Pair. k v))) '() v)]
    (make-s s l)))

(defn merge-s [^Substitutions a ^Substitutions b]
  (when (and a b)
    (cond 
      (identical? a b) a
      (identical? a empty-s) b
      (identical? b empty-s) a
      (< (count (.s b)) (count (.s a)))
        (reduce (fn [ss [u v]] (and ss (unify ss u v))) a (.s b))
      :else (reduce (fn [ss [u v]] (and ss (unify ss u v))) b (.s a)))))

;; =============================================================================
;; Logic Variables

(deftype LVar [name hash cs meta]
  clojure.lang.IObj
  (meta [this]
    meta)
  (withMeta [this new-meta]
    (LVar. name hash cs meta))
  Object
  (toString [_] (str "<lvar:" name ">"))
  (equals [this o]
    (and (.. this getClass (isInstance o))
         (let [^LVar o o]
           (identical? name (.name o)))))
  (hashCode [_] hash)
  ILVar
  (constraints [_] cs)
  (add-constraint [_ c] (LVar. name hash (conj (or cs #{}) c) meta))
  (add-constraints [_ ds] (LVar. name hash (reduce conj (or cs #{}) ds) meta))
  (remove-constraint [_ c] (LVar. name hash (disj cs c) meta))
  (remove-constraints [_] (LVar. name hash nil meta))
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
  IReifyTerm
  (reify-term [v s]
    (ext s v (reify-lvar-name s)))
  IWalkTerm
  (walk-term [v s] v)
  IOccursCheckTerm
  (occurs-check-term [v x s] (= (walk s v) x))
  IBuildTerm
  (build-term [u s]
    (let [m (.s ^Substitutions s)
          l (.l ^Substitutions s)
          lv (lvar 'ignore) ]
      (if (contains? m u)
        s
        (make-s (assoc m u lv)
                (conj l (Pair. u lv)))))))

(defn ^LVar lvar
  ([]
     (let [name (str (. clojure.lang.RT (nextID)))]
       (LVar. name (.hashCode name) nil nil)))
  ([name]
     (let [name (str name "_" (. clojure.lang.RT (nextID)))]
       (LVar. name (.hashCode name) nil nil)))
  ([name cs]
     (let [name (str name "_" (. clojure.lang.RT (nextID)))]
       (LVar. name (.hashCode name) cs nil))))

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
                    (.. this getClass (isInstance d)) (str "(" a " " (toShortString d) ")")
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
  (unify-with-nil [v u s] nil)
  IUnifyWithObject
  (unify-with-object [v u s] nil)
  IUnifyWithLSeq
  (unify-with-lseq [v u s]
    (loop [u u v v s s]
      (if (lvar? u)
        (unify s u v)
        (cond
         (lvar? v) (unify s v u)
         (and (lcons? u) (lcons? v))
           (when-let [s (unify s (lfirst u) (lfirst v))]
             (recur (lnext u) (lnext v) s))
         :else (unify s u v)))))
  IUnifyWithSequential
  (unify-with-seq [v u s]
    (unify-with-lseq u v s))
  IUnifyWithMap
  (unify-with-map [v u s] nil)
  IUnifyWithSet
  (unify-with-set [v u s] nil)
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
  "Constructs a sequence from 2 or more arguments, with the last argument as the tail.
  The tail is improper if the last argument is a logic variable."
  ([f s] `(lcons ~f ~s))
  ([f s & rest] `(lcons ~f (llist ~s ~@rest))))

;; =============================================================================
;; Unification

;; TODO : a lot of cascading ifs need to be converted to cond

(extend-protocol IUnifyTerms
  nil
  (unify-terms [u v s]
    (unify-with-nil v u s)))

(extend-type Object
  IUnifyTerms
  (unify-terms [u v s]
    (unify-with-object v u s)))

(extend-protocol IUnifyTerms
  clojure.lang.Sequential
  (unify-terms [u v s]
    (unify-with-seq v u s)))

(extend-protocol IUnifyTerms
  clojure.lang.IPersistentMap
  (unify-terms [u v s]
    (unify-with-map v u s)))

(extend-protocol IUnifyTerms
  clojure.lang.IPersistentSet
  (unify-terms [u v s]
    (unify-with-set v u s)))

;; -----------------------------------------------------------------------------
;; Unify nil with X

(extend-protocol IUnifyWithNil
  nil
  (unify-with-nil [v u s] s))

(extend-type Object
  IUnifyWithNil
  (unify-with-nil [v u s] nil))

;; -----------------------------------------------------------------------------
;; Unify Object with X

(extend-protocol IUnifyWithObject
  nil
  (unify-with-object [v u s] nil))

(extend-type Object
  IUnifyWithObject
  (unify-with-object [v u s]
    (when (= u v) s)))

;; -----------------------------------------------------------------------------
;; Unify LVar with X

(extend-protocol IUnifyWithLVar
  nil
  (unify-with-lvar [v u s] (ext-no-check s u v)))

(extend-type Object
  IUnifyWithLVar
  (unify-with-lvar [v u s]
    (ext s u v)))

;; -----------------------------------------------------------------------------
;; Unify LCons with X

(extend-protocol IUnifyWithLSeq
  nil
  (unify-with-lseq [v u s] nil))

(extend-type Object
  IUnifyWithLSeq
  (unify-with-lseq [v u s] nil))

(extend-protocol IUnifyWithLSeq
  clojure.lang.Sequential
  (unify-with-lseq [v u s]
    (loop [u u v v s s]
      (if (seq v)
        (if (lcons? u)
          (when-let [s (unify s (lfirst u) (first v))]
            (recur (lnext u) (next v) s))
          (unify s u v))
        (when (lvar? u)
          (unify s u '()))))))

;; -----------------------------------------------------------------------------
;; Unify Sequential with X

(extend-protocol IUnifyWithSequential
  nil
  (unify-with-seq [v u s] nil))

(extend-type Object
  IUnifyWithSequential
  (unify-with-seq [v u s] nil))

(extend-protocol IUnifyWithSequential
  clojure.lang.Sequential
  (unify-with-seq [v u s]
    (loop [u u v v s s]
      (if (seq u)
        (when (seq v)
          (when-let [s (unify s (first u) (first v))]
            (recur (next u) (next v) s)))
        (if (seq v) nil s)))))

;; -----------------------------------------------------------------------------
;; Unify IPersistentMap with X

(extend-protocol IUnifyWithMap
  nil
  (unify-with-map [v u s] nil))

(extend-type Object
  IUnifyWithMap
  (unify-with-map [v u s] nil))

(extend-protocol IUnifyWithMap
  clojure.lang.IPersistentMap
  (unify-with-map [v u s]
    (let [ks (keys u)]
      (loop [ks ks u u v v s s]
        (if (seq ks)
          (let [kf (first ks)
                vf (get v kf ::not-found)]
            (when-not (= vf ::not-found)
              (when-let [s (unify s (get u kf) vf)]
                (recur (next ks) (dissoc u kf) (dissoc v kf) s))))
          (when-not (seq v)
            s))))))

;; -----------------------------------------------------------------------------
;; Unify IPersistentSet with X

(extend-protocol IUnifyWithSet
  nil
  (unify-with-set [v u s] nil))

(extend-type Object
  IUnifyWithSet
  (unify-with-set [v u s] nil))

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

(extend-protocol IUnifyWithSet
  clojure.lang.IPersistentSet
  (unify-with-set [v u s]
    (loop [u u v v ulvars [] umissing []]
      (if (seq u)
        (when (seq v)
          (let [uf (first u)]
            (if (lvar? uf)
              (recur (disj u uf) v (conj ulvars uf) umissing)
              (if (contains? v uf)
                (recur (disj u uf) (disj v uf) ulvars umissing)
                (recur (disj u uf) v ulvars (conj umissing uf))))))
        (if (seq v)
          (when (seq ulvars)
            (loop [v v vlvars [] vmissing []]
              (if (seq v)
                (let [vf (first v)]
                  (if (lvar? vf)
                    (recur (disj v vf) (conj vlvars vf) vmissing)
                    (recur (disj v vf) vlvars (conj vmissing vf))))
                (unify s (concat ulvars umissing)
                       (concat vmissing vlvars)))))
          s)))))

;; =============================================================================
;; Reification

(extend-protocol IReifyTerm
  nil
  (reify-term [v s] s))

(extend-type Object
  IReifyTerm
  (reify-term [v s] s))

(extend-protocol IReifyTerm
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
  (walk-term [v s] nil))

(extend-type Object
  IWalkTerm
  (walk-term [v s] v))

(extend-protocol IWalkTerm
  clojure.lang.ISeq
  (walk-term [v s]
    (map #(walk* s %) v)))

(extend-protocol IWalkTerm
  clojure.lang.IPersistentVector
  (walk-term [v s]
    (loop [v v r (transient [])]
      (if (seq v)
        (recur (next v) (conj! r (walk* s (first v))))
        (persistent! r)))))

(extend-protocol IWalkTerm
  clojure.lang.IPersistentMap
  (walk-term [v s]
    (loop [v v r (transient {})]
      (if (seq v)
        (let [[vfk vfv] (first v)]
          (recur (next v) (assoc! r vfk (walk* s vfv))))
        (persistent! r)))))

(extend-protocol IWalkTerm
  clojure.lang.IPersistentSet
  (walk-term [v s]
    (loop [v v r {}]
      (if (seq v)
        (recur (next v) (conj r (walk* s (first v))))
        r))))

;; =============================================================================
;; Occurs Check Term

(extend-protocol IOccursCheckTerm
  nil
  (occurs-check-term [v x s] false))

(extend-type Object
  IOccursCheckTerm
  (occurs-check-term [v x s] false))

(extend-protocol IOccursCheckTerm
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
  (build-term [u s] s))

(extend-type Object
  IBuildTerm
  (build-term [u s] s))

(extend-protocol IBuildTerm
  clojure.lang.ISeq
  (build-term [u s]
    (reduce build s u)))

;; =============================================================================
;; Goals and Goal Constructors

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

;; -----------------------------------------------------------------------------
;; Zero for join, identity for plus

(extend-type nil
  Search
  (yield [_] nil)
  (step [_] nil)
  (min-yield [_] nil)
  (restrict [_ _] nil)
  (sizehint [_] 0))

;; -----------------------------------------------------------------------------
;; Inc

(declare plus join narrow bind)

(defn- step+
  "Repeatedly calls step+ as long as there's no yield and that sizehint 
   decreases (there is allowance for short plateaux)." 
  [a]
  (loop [a a sza (sizehint a) prob true]
    (let [sa (step a)
          szsa (sizehint sa)
          d (- szsa sza)]
      (cond
        (yield sa) sa
        (neg? d) (recur sa szsa true)
        (and prob (zero? d)) (recur sa szsa false)
        :else sa))))

(deftype Plus [a b y min sz]
  Search
  (yield [this] y)
  (step [this]
    (plus b (step+ a) min))
  (min-yield [this] min)
  (restrict [this ss] (plus (narrow a ss) (narrow b ss) ss))
  (sizehint [this] sz))

(defn plus 
  "Returns the union of two Searches."
  [a b miny]
  (or (and a b (Plus. a b (yield a) miny (+ (sizehint a) (sizehint b)))) a b))

(deftype Join [a b min sz]
  Search
  (yield [this] nil)
  (step [this]
    (plus (narrow b (yield a))
          (join b (step+ a) min)
          min))
  (min-yield [this] min)
  (restrict [this ss] (join (narrow a ss) (narrow b ss) ss))
  (sizehint [this] sz))

(defn join 
  "Returns the intersection of two Searches"
  [a b min]
  (and a b (Join. a b min (+ (sizehint a) (sizehint b)))))

;; the min field of choice, plus and join may be removed and replaced by narrow
;; instances
(deftype Narrow [a ss]
  Search
  (yield [this] (merge-s (yield a) ss))
  (step [this] (narrow (step+ (restrict a ss)) ss))
  (min-yield [this] ss)
  (restrict [this oss] (narrow a oss))
  (sizehint [this] (sizehint a)))

(defn narrow
  "Restrict the search to substitutions greater than or equal to ss.
   Semi-lazy: ss is checked to be unifiable with min-yield eagerly so as to
   fail fast but the actual narrowing happens lazily." 
  [a ss]
  (when-let [uss (min-yield a)]
    (when-let [nss (merge-s ss uss)]
      (if (= nss uss)
        a
        (Narrow. a nss)))))

(deftype Bind [a f]
  Search
  (yield [this] nil)
  (step [this]
    (plus (when-let [ss (yield a)] (f ss)) 
          (bind (step a) f)
          (min-yield a)))
  (min-yield [this] (min-yield a))
  (restrict [this ss]
    (bind (narrow a ss) f))
  (sizehint [this] (sizehint a)))

(defn bind [a f]
  (and a (Bind. a f)))

(extend-type clojure.lang.Fn
  Search
  (yield [this]
    nil)
  (step [this] (this))
  (min-yield [this] empty-s)
  (restrict [this ss] this)
  (sizehint [this] 1))

(extend-type Object
  Search
  (yield [this]
    (first this))
  (step [this]
    (next this))
  (min-yield [this] empty-s)
  (restrict [this ss] this)
  (sizehint [this] 1))

;; =============================================================================
;; Syntax

(defmacro goal {:arglists '([name? ss-binding yield-expr? stepexpr])} 
  ([& args]
    (let [[name [ss] & exprs] (if (symbol? (first args)) 
                          args
                          (cons (gensym "goal") args))
          [yield-expr stepexpr] (if (next exprs)
                                   [nil (first exprs)]
                                   exprs)]
      `((fn goal# [~ss]
          (reify Search
            (yield [~name] ~yield-expr)
            (step [~name] ~stepexpr)
            (min-yield [_#] ~ss)
            (restrict [_# ss#] (goal# ss#))
            (sizehint [_#] 1)))
         empty-s))))

(def succeed
  "A goal that always succeeds."
  (goal succeed [ss] ss succeed))

(def fail
  "A goal that always fails."
  nil)

(def s# succeed)

(def u# fail)

;; is this really better than the restrict-oblivious fn closure below?
(defmacro ==
  "A goal that attempts to unify terms u and v."
  [u v]
  `(goal [ss#] (unify ss# ~u ~v) nil))

#_(defmacro ==
  "A goal that attempts to unify terms u and v."
  [u v]
  `(fn [] (unify empty-s ~u ~v)))

(defn- calltree [items op]
  (reduce #(list op %1 %2 `empty-s) (reverse items)))

(defmacro conde
  "Logical disjunction of the clauses. The first goal in
  a clause is considered the head of that clause. Interleaves the
  execution of the clauses."
  [& clauses]
  (let [clauses (map #(calltree % `join) clauses)
        clauses (calltree clauses `plus)]
    `(fn [] ~clauses)
    clauses))

(defn- lvar-bind [sym]
  [sym `(lvar '~sym)])

(defn- lvar-binds [syms]
  (mapcat lvar-bind syms))

(defmacro fresh
  "Creates fresh variables. Goals occuring within form a logical 
  conjunction."
  [[& lvars] & goals]
  (if (seq goals)
    `(fn []
       (let [~@(lvar-binds lvars)]
         ~(calltree goals `join)))
    `succeed))

(defn ss-seq 
  "Returns a lazy sequence of the substitutions yielded by a." [a]
  (lazy-seq ((fn ss-seq* [a]
               (when a
                 (if-let [ss (yield a)]
                   (cons ss (lazy-seq (ss-seq* (step a))))
                   (recur (step a))))) 
              a)))

(defmacro solve [& [n [x] & goals]]
  `(let [~@(lvar-bind x)
         sss# (ss-seq (all ~@goals))
         xs# (map (fn [ss#] (-reify ss# ~x)) sss#)]
     (if ~n
       (take ~n xs#)
       xs#)))

(defmacro run
  "Executes goals until a maximum of n results are found."
  [n & goals]
  `(doall (solve ~n ~@goals)))

(defmacro run*
  "Executes goals until results are exhausted."
  [& goals]
  `(run false ~@goals))

(defmacro run-nc
  "Executes goals until a maximum of n results are found. Does not occurs-check."
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
  [& goals] `(fresh [] ~@goals))

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

(defn- rem-? [s]
  (symbol (apply str (drop 1 (str s)))))

(defn- proc-lvar [lvar-expr store]
  (let [v (if-let [u (@store lvar-expr)]
            u
            (lvar (rem-? lvar-expr)))]
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

(defn- prep
  "Prep a quoted expression. All symbols preceded by ? will
  be replaced with logic vars."
  [expr]
  (let [lvars (atom {})
        prepped (if (lcons-expr? expr)
                  (prep* expr lvars true)
                  (postwalk (replace-lvar lvars) expr))]
    (with-meta prepped {:lvars @lvars})))

(defn- unifier*
  "Unify the terms u and w."
  [u w]
  
  (first
    (run* [q]
      (== u w)
      (== u q))))

(defn- binding-map*
  "Return the binding map that unifies terms u and w.
  u and w should prepped terms."
  [u w]
  (let [lvars (merge (-> u meta :lvars)
                     (-> w meta :lvars))
        s (unify empty-s u w)]
    (when s
      (into {} (map (fn [[k v]]
                      [k (-reify s v)])
                    lvars)))))

(defn unifier
  "Unify the terms u and w. Will prep the terms."
  [u w]
  {:pre [(not (lcons? u))
         (not (lcons? w))]}
  (let [up (prep u)
        wp (prep w)]
    (unifier* up wp)))

(defn binding-map
  "Return the binding map that unifies terms u and w.
  Will prep the terms."
  [u w]
  {:pre [(not (lcons? u))
         (not (lcons? w))]}
  (let [up (prep u)
        wp (prep w)]
    (binding-map* up wp)))

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

(defn ifa [q then else]
  (if q
    (goal [s]
      (if-let [qs (yield q)]
        (plus (narrow then qs) (join (step q) then s) s)
        (ifa (step q) (narrow then s) (narrow else s))))
    else))

(defn ifu [q then else]
  (if q
    (goal [s]
      (if-let [qs (yield q)]
        (narrow then qs)
        (ifu (step q) (narrow then s) (narrow else s))))
    else))

;; TODO : if -> when

(defmacro ifa*
  ([])
  ([[e & gs] & grest]
     `(ifa ~e (bind* ~@gs)
           `(ifa* ~@grest))))

(defmacro ifu*
  ([])
  ([[e & gs] & grest]
     `(ifu ~e (bind* ~@gs)
           `(ifu* ~@grest))))

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
   (and (coll? p)
        (not= (first p) 'quote)) (cond
                                  (list? p) p
                                  :else `[~@(map p->term p)])
   :else p))

(defn- lvar-sym? [s]
  (and (symbol? s)
       (not= s '.)
       (not (contains? *locals* s))))

(defn- extract-vars
  ([p]
     (set (cond
           (lvar-sym? p) [p]
           (coll? p) (filter lvar-sym? (flatten p))
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

;; ==============================================================================
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

;; =============================================================================
;; Rel

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
            (~'clojure.core.logic.Rel. '~name (atom {}) nil ~@(map arity-excs r))
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

#_(defmacro RelHelper [arity]
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
         (~'applyTo [~'tfhis ~'arglist]
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

#_(RelHelper 20)

(defn- index-sym [name arity o]
  (->sym name "_" arity "-" o "-index"))

(defn- set-sym [name arity]
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
                       `((not (clojure.core.logic/lvar? (clojure.core.logic/walk* ~'a ~a)))
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
                                    ~'a)))
                           (remove nil?)))))))))))

;; TODO: Should probably happen in a transaction

#_(defn facts
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
           (let [index (var-get (resolve (index-sym (.name rel) arity o)))]
             (let [indexed-tuples (map (fn [t]
                                         {(nth t (dec i)) #{t}})
                                       tuples)]
               (swap! index
                      (fn [i]
                        (apply merge-with set/union i indexed-tuples))))))))))

#_(defn fact
  "Add a fact to a relation defined with defrel."
  [rel & tuple]
  (facts rel [(vec tuple)]))

;; =============================================================================
;; Tabling

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

#_(defn w-check [w sk fk]
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

#_(
(defprotocol ITabled
  (-reify-tabled [this v])
  (reify-tabled [this v])
  (alpha-equiv? [this x y])
  (reuse [this argv cache start end])
  (subunify [this arg ans]))

;; CONSIDER: subunify, reify-term-tabled, extending all the necessary types to them

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
                       (Choice. (subunify this argv (reify-tabled this (first ansv*)))
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
                        (mplus a-inf (fn [] this))))))))

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
                (bind a (fresh []
                          (apply goal args)
                          (master argv cache))))
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
                 (bind a# (fresh []
                            ~@grest
                            (master argv# cache#))))
               (reuse a# argv# cache# nil nil)))))))))

;; =============================================================================
;; Disequality

;; TODO: change to lazy-seq
(defn prefix [s <s]
  (if (= s <s)
    ()
    (cons (first s) (prefix (rest s) <s))))

;; =============================================================================
;; Verification

(defprotocol IConstraintStore
  (merge-constraint [this c])
  (refine-constraint [this c u])
  (discard-constraint [this c])
  (propagate [this s u v])
  (get-simplified [this])
  (discard-simplified [this]))

(defprotocol IDisequality
  (!=-verify [this sp]))

(declare make-store)
(declare make-c)
(declare constraint-verify)

(defn ^Substitutions constraint-verify-simple [^Substitutions s u v]
  (let [uc (set (map #(walk s %) (constraints u)))]
    (if (contains? uc v)
      nil
      (let [u (remove-constraints u)
            v (if (lvar? v) (add-constraints v uc) v)]
        (make-s (-> (.s s) (dissoc u) (assoc u v))
                (conj (.l s) (pair u v))
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

(defmacro !=
  "Impose a disequality constraint on u and v. If the two
  terms ever unify will result in failure. u and v can be
  compound terms allowing complex conditions to require
  failure."
  [u v]
  `(fn [a#]
     (!=-verify a# (unify a# ~u ~v))))
