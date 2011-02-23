(ns logos.minikanren
  (:refer-clojure :exclude [reify ==])
  (:use [clojure.pprint :only [pprint]]))

(def ^:dynamic *occurs-check* true)

;; =============================================================================
;; Logic Variables

(deftype LVar [name s]
  Object
  (toString [this] (str "<lvar:" name ">")))

(defn ^LVar lvar
  ([] (LVar. (gensym) nil))
  ([name] (LVar. name nil))
  ([name s] (LVar. name s)))

(defmethod print-method LVar [x writer]
  (.write writer (str "<lvar:" (.name ^LVar x) ">")))

(defn lvar? [x]
  (instance? LVar x))

;; =============================================================================
;; LCons

(defprotocol LConsSeq
  (lfirst [this])
  (lnext [this]))

;; TODO: clean up the printing code

(defprotocol LConsPrint
  (toShortString [this]))

(deftype LCons [a d cache]
  LConsSeq
  (lfirst [_] a)
  (lnext [_] d)
  LConsPrint
  (toShortString [this]
                 (cond
                  (instance? LCons d) (str a " " (toShortString d))
                  :else (str a " . " d )))
  Object
  (toString [this] (cond
                    (instance? LCons d) (str "(" a " " (toShortString d) ")")
                    :else (str "(" a " . " d ")")))
  (equals [this o]
          (or (identical? this o)
              (and (instance? LCons o)
                   (loop [me this
                          you o]
                     (cond
                      (nil? me) (nil? you)
                      (lvar? me) true
                      (lvar? you) true
                      :else (let [mef  (lfirst me)
                                  youf (lfirst you)]
                              (and (or (= mef youf)
                                       (lvar? mef)
                                       (lvar? youf))
                                   (recur (lnext me) (lnext you)))))))))

  (hashCode [this]
            (if @cache
              @cache
              (loop [hash 1 xs this]
                (if (or (nil? xs) (lvar? xs))
                  (reset! cache hash)
                  (let [val (lfirst xs)]
                    (recur (unchecked-add-int
                            (unchecked-multiply-int 31 hash)
                            (clojure.lang.Util/hash val))
                           (lnext xs))))))))

(defmethod print-method LCons [x writer]
  (.write writer (str x)))

(defn lcons [a d]
  (if (or (coll? d) (nil? d))
    (cons a (seq d))
    (LCons. a d (atom nil))))

(defmacro llist
  ([f s] `(lcons ~f ~s))
  ([f s & rest] `(lcons ~f (llist ~s ~@rest))))

;; =============================================================================
;; Substitutions

(defprotocol ISubstitutions
  (length [this])
  (occurs-check [this u v])
  (ext [this x v])
  (ext-no-check [this x v])
  (walk [this v])
  (walk* [this v])
  (unify [this u v])
  (reify-lvar-name [_])
  (-reify [this v])
  (reify [this v])
  (build [this u]))

(declare empty-s)
(declare unify-terms)
(declare occurs-check-term)
(declare reify-term)
(declare walk-term)
(declare build-term)

(deftype Substitutions [s]
  Object
  (equals [this o]
    (or (identical? this o)
        (and (instance? Substitutions o)
             (= s ^clojure.lang.PersistentHashMap (.s ^Substitutions o)))))

  ISubstitutions
  (length [this] (count s))

  (occurs-check [this x v]
                (let [v (walk this v)]
                  (occurs-check-term v x this)))
  
  (ext [this x v]
       (if (and *occurs-check* (occurs-check this x v))
         this
         (ext-no-check this x v)))

  (ext-no-check [this x v]
                (Substitutions. (assoc s x v)))

  (walk [this v]
        (loop [v v lv nil]
          (cond
           (identical? v ::not-found) lv
           (not (lvar? v)) v
           :else (recur (get s v ::not-found) v))))

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

  (-reify [this v]
          (let [v (walk this v)]
            (reify-term v this)))

  (reify [this v]
         (let [v (walk* this v)]
           (walk* (-reify empty-s v) v)))

  (build [this u]
         (build-term u this)))

(def empty-s (Substitutions. {}))

(defn subst? [x]
  (instance? Substitutions x))

(defn to-s [v]
  (let [s (reduce (fn [m [k v]] (assoc m k v)) {} v)]
    (Substitutions. s)))

;; =============================================================================
;; Unification

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

(extend-protocol IUnifyTerms
  nil
  (unify-terms [u v s]
    (unify-with-nil v u s)))

(extend-type Object
  IUnifyTerms
  (unify-terms [u v s]
    (unify-with-object v u s)))

(extend-type LVar
  IUnifyTerms
  (unify-terms [u v s]
    (unify-with-lvar v u s)))

(extend-type LCons
  IUnifyTerms
  (unify-terms [u v s]
    (unify-with-lseq v u s)))

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
  (unify-with-nil [v u s] false))

(extend-type LVar
  IUnifyWithNil
  (unify-with-nil [v u s]
    (ext-no-check s v u)))

(extend-type LCons
  IUnifyWithNil
  (unify-with-nil [v u s] false))

(extend-protocol IUnifyWithNil
  clojure.lang.Sequential
  (unify-with-nil [v u s] false))

(extend-protocol IUnifyWithNil
  clojure.lang.IPersistentMap
  (unify-with-nil [v u s] false))

(extend-protocol IUnifyWithNil
  clojure.lang.IPersistentSet
  (unify-with-nil [v u s] false))

;; -----------------------------------------------------------------------------
;; Unify Object with X

(extend-protocol IUnifyWithObject
  nil
  (unify-with-object [v u s] false))

(extend-type Object
  IUnifyWithObject
  (unify-with-object [v u s]
    (if (= u v) s false)))

(extend-type LVar
  IUnifyWithObject
  (unify-with-object [v u s]
    (ext s v u)))

(extend-type LCons
  IUnifyWithObject
  (unify-with-object [v u s] false))

(extend-protocol IUnifyWithObject
  clojure.lang.Sequential
  (unify-with-object [v u s] false))

(extend-protocol IUnifyWithObject
  clojure.lang.IPersistentMap
  (unify-with-object [v u s] false))

(extend-protocol IUnifyWithObject
  clojure.lang.IPersistentSet
  (unify-with-object [v u s] false))

;; -----------------------------------------------------------------------------
;; Unify LVar with X

(extend-protocol IUnifyWithLVar
  nil
  (unify-with-lvar [v u s] (ext-no-check s u v)))

(extend-type Object
  IUnifyWithLVar
  (unify-with-lvar [v u s]
    (ext s u v)))

(extend-type LVar
  IUnifyWithLVar
  (unify-with-lvar [v u s]
    (ext-no-check s u v)))

(extend-type LCons
  IUnifyWithLVar
  (unify-with-lvar [v u s]
    (ext s u v)))

(extend-protocol IUnifyWithLVar
  clojure.lang.Sequential
  (unify-with-lvar [v u s]
    (ext s u v)))

(extend-protocol IUnifyWithLVar
  clojure.lang.IPersistentMap
  (unify-with-lvar [v u s]
    (ext s u v)))

(extend-protocol IUnifyWithLVar
  clojure.lang.IPersistentSet
  (unify-with-lvar [v u s]
    (ext s u v)))

;; -----------------------------------------------------------------------------
;; Unify LCons with X

(extend-protocol IUnifyWithLSeq
  nil
  (unify-with-lseq [v u s] false))

(extend-type Object
  IUnifyWithLSeq
  (unify-with-lseq [v u s] false))

(extend-type LVar
  IUnifyWithLSeq
  (unify-with-lseq [v u s]
    (ext s v u)))

(extend-type LCons
  IUnifyWithLSeq
  (unify-with-lseq [v u s]
    (loop [u u v v s s]
      (if (lvar? u)
        (ext s u v)
        (if (lvar? v)
          (ext s v u)
          (if-let [s (unify s (lfirst u) (lfirst v))]
            (recur (lnext u) (lnext v) s)
            false))))))

(extend-protocol IUnifyWithLSeq
  clojure.lang.Sequential
  (unify-with-lseq [v u s]
    (loop [u u v v s s]
      (if (seq v)
        (if (lvar? u)
          (ext s u v)
          (if-let [s (unify s (lfirst u) (first v))]
            (recur (lnext u) (next v) s)
            false))
        (if (lvar? u)
          (ext-no-check s u '())
          false)))))

(extend-protocol IUnifyWithLSeq
  clojure.lang.IPersistentMap
  (unify-with-lseq [v u s] false))

(extend-protocol IUnifyWithLSeq
  clojure.lang.IPersistentSet
  (unify-with-lseq [v u s] false))

;; -----------------------------------------------------------------------------
;; Unify Sequential with X

(extend-protocol IUnifyWithSequential
  nil
  (unify-with-seq [v u s] false))

(extend-type Object
  IUnifyWithSequential
  (unify-with-seq [v u s] false))

(extend-type LVar
  IUnifyWithSequential
  (unify-with-seq [v u s]
    (ext s v u)))

(extend-type LCons
  IUnifyWithSequential
  (unify-with-seq [v u s]
    (unify-with-lseq u v s)))

(extend-protocol IUnifyWithSequential
  clojure.lang.IPersistentMap
  (unify-with-seq [v u s] false))

(extend-protocol IUnifyWithSequential
  clojure.lang.IPersistentSet
  (unify-with-seq [v u s] false))

(extend-protocol IUnifyWithSequential
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
  (unify-with-map [v u s] false))

(extend-type Object
  IUnifyWithMap
  (unify-with-map [v u s] false))

(extend-type LVar
  IUnifyWithMap
  (unify-with-map [v u s]
    (ext s v u)))

(extend-type LCons
  IUnifyWithMap
  (unify-with-map [v u s] false))

(extend-protocol IUnifyWithMap
  clojure.lang.Sequential
  (unify-with-map [v u s] false))

(extend-protocol IUnifyWithMap
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

(extend-protocol IUnifyWithMap
  clojure.lang.IPersistentSet
  (unify-with-map [v u s] false))

;; -----------------------------------------------------------------------------
;; Unify IPersistentSet with X

(extend-protocol IUnifyWithSet
  nil
  (unify-with-set [v u s] false))

(extend-type Object
  IUnifyWithSet
  (unify-with-set [v u s] false))

(extend-type LVar
  IUnifyWithSet
  (unify-with-set [v u s]
    (ext s v u)))

(extend-type LCons
  IUnifyWithSet
  (unify-with-set [v u s] false))

(extend-protocol IUnifyWithSet
  clojure.lang.Sequential
  (unify-with-set [v u s] false))

(extend-protocol IUnifyWithSet
  clojure.lang.IPersistentMap
  (unify-with-set [v u s] false))

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

;; =============================================================================
;; Reification

(defprotocol IReifyTerm
  (reify-term [v s]))

(extend-type Object
  IReifyTerm
  (reify-term [v s] s))

(extend-type LVar
  IReifyTerm
  (reify-term [v s]
    (ext s v (reify-lvar-name s))))

(extend-type LCons
  IReifyTerm
  (reify-term [v s]
    (loop [v v s s]
      (if (lvar? v)
        (-reify s v)
        (recur (lnext v) (-reify s (lfirst v)))))))

(extend-protocol IReifyTerm
  clojure.lang.IPersistentCollection
  (reify-term [v s]
    (loop [v v s s]
      (if (seq v)
        (recur (next v) (-reify s (first v)))
        s))))

;; =============================================================================
;; Walk Term

(defprotocol IWalkTerm
  (walk-term [v s]))

(extend-type Object
  IWalkTerm
  (walk-term [v s] v))

(extend-type LVar
  IWalkTerm
  (walk-term [v s] v))

;; TODO: no way to make this non-stack consuming w/o a lot more thinking
;; we could use continuation passing style and trampoline

(extend-type LCons
  IWalkTerm
  (walk-term [v s]
    (lcons (walk* s (lfirst v))
           (walk* s (lnext v)))))

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

(defprotocol IOccursCheckTerm
  (occurs-check-term [v x s]))

(extend-type Object
  IOccursCheckTerm
  (occurs-check-term [v x s] false))

(extend-type LVar
  IOccursCheckTerm
  (occurs-check-term [v x s] (= (walk s v) x)))

(extend-type LCons
  IOccursCheckTerm
  (occurs-check-term [v x s]
    (loop [v v x x s s]
      (if (lvar? v)
        (occurs-check s x v)
        (or (occurs-check s x (lfirst v))
            (recur (lnext v) x s))))))

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

(defprotocol IBuildTerm
  (build-term [u s]))

(extend-protocol IBuildTerm
  nil
  (build-term [u s] s))

(extend-type Object
  IBuildTerm
  (build-term [u s] s))

(extend-type LVar
  IBuildTerm
  (build-term [u s]
   (let [m (.s ^Substitutions s)]
     (if (contains? m u)
       s
       (Substitutions. (assoc m u (lvar 'ignore)))))))

(extend-type LCons
  IBuildTerm
  (build-term [u s]
     (loop [u u s s]
       (if (lvar? u)
         (build s u)
         (recur (lnext u) (build s (lfirst u)))))))

(extend-protocol IBuildTerm
  clojure.lang.ISeq
  (build-term [u s]
    (reduce build s u)))

;; =============================================================================
;; Goals and Goal Constructors

(defprotocol IBind
  (bind [this g]))

(defprotocol IMPlus
  (mplus [a b]))

(defmacro bind*
  ([a g] `(bind ~a ~g))
  ([a g & g-rest]
     `(bind* (bind ~a ~g) ~@g-rest)))

(defmacro mplus*
  ([e] e)
  ([e & e-rest]
     `(mplus ~e (lazy-seq (let [r# (mplus* ~@e-rest)]
                            (cond
                             (subst? r#) (cons r# nil)
                             :else r#))))))

;; -----------------------------------------------------------------------------
;; MZero

(extend-protocol IBind
  nil
  (bind [_ g] nil))

(extend-protocol IMPlus
  nil
  (mplus [_ b] b))

;; -----------------------------------------------------------------------------
;; Unit

(defprotocol IMPlusUnit
  (mplus-u [this a]))

(extend-type Substitutions
  IBind
  (bind [this g]
        (g this))
  IMPlus
  (mplus [this b]
         (mplus-u b this)))

(extend-protocol IMPlusUnit
  nil
  (mplus-u [_ a] a))

(extend-type Substitutions
  IMPlusUnit
  (mplus-u [this a]
           (list a this)))

(extend-protocol IMPlusUnit
  clojure.lang.ISeq
  (mplus-u [this a]
           (cons a this)))

;; -----------------------------------------------------------------------------
;; Stream

(defn mplus-concat [[f & r]]
  (if r
    (lazy-seq
     (mplus f (mplus-concat r)))
    f))

(extend-protocol IBind
  clojure.lang.ISeq
  (bind [this g]
        (if-let [r (seq (map g this))]
          (mplus-concat r))))

(defprotocol IMPlusStream
  (mplus-s [this a]))

(extend-protocol IMPlus
  clojure.lang.ISeq
  (mplus [this b]
         (mplus-s b this)))

(extend-protocol IMPlusStream
  nil
  (mplus-s [_ a] a))

(extend-type Substitutions
  IMPlusStream
  (mplus-s [this a] (cons this a)))

(extend-protocol IMPlusStream
  clojure.lang.ISeq
  (mplus-s [this a]
           (if (seq this)
             (if (seq a)
               (cons (first a)
                     (cons (first this)
                           (mplus* (next a) (next this))))
               this)
             (if (seq a) a '()))))

;; =============================================================================
;; Syntax

(defn succeed [a] a)

(defn fail [a] nil)

(def s# succeed)

(def u# fail)

(defmacro == [u v]
  `(fn [a#]
     (if-let [b# (unify a# ~u ~v)]
       b#
       nil)))

(defn bind-cond-e-clause [a]
  (fn [g-rest]
    `(bind* ~a ~@g-rest)))

(defn bind-cond-e-clauses [a clauses]
  (map (bind-cond-e-clause a) clauses))

(defmacro cond-e [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (lazy-seq
        (let [s# (mplus* ~@(bind-cond-e-clauses a clauses))]
          (or (and (subst? s#)
                   (list s#))
              s#))))))

(defn lvar-bind [sym]
  ((juxt identity
         (fn [s] `(lvar '~s))) sym))

(defn lvar-binds [syms]
  (mapcat lvar-bind syms))

(defmacro exist [[& x-rest] & g-rest]
  `(fn [a#]
     (let [~@(lvar-binds x-rest)]
       (bind* a# ~@g-rest))))

(defn reifier [lvar]
  (fn [a]
    (reify a lvar)))

(defn to-seq [x]
  (cond
   (nil? x) '()
   (seq? x) x
   :else    (list x)))

(defmacro run [& [n [x] & g-rest]]
  `(let [r# (let [~x (lvar '~x)]
              (->> (to-seq ((fn [a#] (bind* a# ~@g-rest)) empty-s))
                   (remove nil?)
                   (map (reifier ~x))))]
     (if ~n (take ~n r#) r#)))

(defmacro run* [& body]
  `(run false ~@body))

;; TODO : fix if possible, thought scopes could help, but now unsure

(defmacro run-nc [& [n [x] & g-rest]]
  `(binding [*occurs-check* false]
     (doall (run ~n [~x] ~@g-rest))))

(defmacro run-nc* [& body]
  `(run-nc false ~@body))

;; solve is like run except it promises to compute the result immediately
;; run-nc needs to compute its results immediately only because bindings
;; and laziness don't mix at the moment.

(defmacro solve [& [n [x] & g-rest]]
  `(doall (run ~n [~x] ~@g-rest)))

(defmacro solve* [& body]
  `(solve false ~@body))

;; NOTE : solve-nc relies on run-nc, subject to change

(defmacro solve-nc [& body]
  `(run-nc ~@body))

(defmacro solve-nc* [& body]
  `(solve-nc false ~@body))

(defn sym->lvar [sym]
  `(lvar '~sym))

(defmacro all
  ([] `logos.minikanren/s#)
  ([& g-rest] `(fn [a#] (bind* a# ~@g-rest))))

;; =============================================================================
;; Debugging

(def ^:dynamic *debug* (atom []))

(defn reset-debug! []
  (reset! *debug* []))

(defmacro trace [a & lm]
  `(binding [*debug* (or ~a *debug*)]
     ~@lm))

(defn trace-lvar [a lvar]
  `(swap! *debug* conj (format "%5s = %s\n" (str '~lvar) (reify ~a ~lvar))))

(defmacro log [title & exprs]
  `(do
     (swap! *debug* conj (str ~title "\n"))
     (swap! *debug* conj (str ~@exprs "\n"))
     (swap! *debug* conj "\n")))

(defmacro trace-lvars [title & lvars]
  (let [a (gensym "a")]
    `(fn [~a]
       (swap! *debug* conj (str ~title "\n"))
       ~@(map (partial trace-lvar a) lvars)
       (swap! *debug* conj "\n")
       ~a)))

(defn print-debug [a]
  (println (reduce str @a)))

(defmacro trace-s []
  (let [a (gensym "a")]
   `(fn [~a]
      (swap! *debug* conj ~a)
      ~a)))