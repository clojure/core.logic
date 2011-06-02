(ns clojure.core.logic.minikanren
  (:refer-clojure :exclude [reify == inc])
  (:import [java.io Writer]))

(set! *warn-on-reflection* true)

(def ^{:dynamic true} *occurs-check* true)

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

(deftype Unbound [])
(def ^Unbound unbound (Unbound.))

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

(defn ^Pair pair [lhs rhs]
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
  (-reify [this v])
  (reify [this v])
  (build [this u]))

(declare empty-s)
(declare unify-terms)
(declare occurs-check-term)
(declare reify-term)
(declare walk-term)
(declare build-term)
(declare choice)
(declare add-constraint)
(declare lvar)
(declare lvar?)
(declare pair)
(declare lcons)

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
      this
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

  (-reify [this v]
    (let [v (walk this v)]
      (reify-term v this)))

  (reify [this v]
    (let [v (walk* this v)]
      (walk* (-reify empty-s v) v)))

  (build [this u]
    (build-term u this))

  IBind
  (bind [this g]
    (g this))
  IMPlus
  (mplus [this f]
    (choice this f))
  ITake
  (take* [this] this))

(defn ^Substitutions pass-verify [^Substitutions s u v]
  (Substitutions. (assoc (.s s) u v)
                  (cons (pair u v) (.l s))
                  (.verify s)
                  (.cs s)))

(defn ^Substitutions make-s
  ([m l] (Substitutions. m l pass-verify nil))
  ([m l f] (Substitutions. m l f nil))
  ([m l f cs] (Substitutions. m l f cs)))

(def ^Substitutions empty-s (make-s {} '()))

(defn subst? [x]
  (instance? Substitutions x))

(defn ^Substitutions to-s [v]
  (let [s (reduce (fn [m [k v]] (assoc m k v)) {} v)
        l (reduce (fn [l [k v]] (cons (Pair. k v) l)) '() v)]
    (make-s s l)))

;; =============================================================================
;; Logic Variables

(defprotocol ILVar
  (constraints [this])
  (add-constraint [this c])
  (add-constraints [this ds])
  (remove-constraint [this c])
  (remove-constraints [this]))

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
                (cons (Pair. u lv) l))))))

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
  (unify-with-nil [v u s] false)
  IUnifyWithObject
  (unify-with-object [v u s] false)
  IUnifyWithLVar
  (unify-with-lvar [v u s]
    (ext s u v))
  IUnifyWithLSeq
  (unify-with-lseq [v u s]
    (loop [u u v v s s]
      (if (lvar? u)
        (ext s u v)
        (cond
         (lvar? v) (ext s v u)
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
        (recur (lnext v) (-reify s (lfirst v)))
        (-reify s v))))
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

(defn lcons [a d]
  (if (or (coll? d) (nil? d))
    (cons a (seq d))
    (LCons. a d -1 nil)))

(defn lcons? [x]
  (instance? LCons x))

(defmacro llist
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

(extend-protocol IUnifyWithLSeq
  clojure.lang.Sequential
  (unify-with-lseq [v u s]
    (loop [u u v v s s]
      (if (seq v)
        (if (lcons? u)
          (if-let [s (unify s (lfirst u) (first v))]
            (recur (lnext u) (next v) s)
            false)
          (ext s u v))
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
        (recur (next v) (-reify s (first v)))
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

(defmacro inc [& rest]
  `(fn inc [] ~@rest))

(extend-type Object
  ITake
  (take* [this] this))

;; TODO: Choice always holds a as a list, can we just remove that?

(deftype Choice [a f]
  IBind
  (bind [this g]
    (mplus (g a) (inc (bind f g))))
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
    (inc (bind (this) g)))
  IMPlus
  (mplus [this f]
    (inc (mplus (f) this)))
  ITake
  (take* [this] (lazy-seq (take* (this)))))

;; =============================================================================
;; Syntax

(defn succeed [a] a)

(defn fail [a] nil)

(def s# succeed)

(def u# fail)

(defmacro == [u v]
  `(fn [a#]
     (if-let [b# (unify a# ~u ~v)]
       b# nil)))

(defn bind-conde-clause [a]
  (fn [g-rest]
    `(bind* ~a ~@g-rest)))

(defn bind-conde-clauses [a clauses]
  (map (bind-conde-clause a) clauses))

(defmacro conde [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (inc
        (mplus* ~@(bind-conde-clauses a clauses))))))

(defn lvar-bind [sym]
  ((juxt identity
         (fn [s] `(lvar '~s))) sym))

(defn lvar-binds [syms]
  (mapcat lvar-bind syms))

(defmacro exist [[& x-rest] & g-rest]
  `(fn [a#]
     (inc
      (let [~@(lvar-binds x-rest)]
        (bind* a# ~@g-rest)))))

(defmacro run [& [n [x] & g-rest]]
  `(let [xs# (take* (fn []
                      ((exist [~x] ~@g-rest
                         (fn [a#]
                           (cons (reify a# ~x) '()))) ;; TODO: do we need this?
                       empty-s)))]
     (if ~n
       (take ~n xs#)
       xs#)))

(defmacro run* [& body]
  `(run false ~@body))

(defmacro run-nc [& [n [x] & g-rest]]
  `(binding [*occurs-check* false]
     (run ~n [~x] ~@g-rest)))

(defmacro run-nc* [& body]
  `(run-nc false ~@body))

(defmacro run-debug [& body]
  `(doall
    (run ~@body)))

(defmacro run-debug* [& body]
  `(doall
    (run* ~@body)))

(defn sym->lvar [sym]
  `(lvar '~sym))

(defmacro all
  ([] `clojure.core.logic.minikanren/s#)
  ([& g-rest] `(fn [a#] (bind* a# ~@g-rest))))

;; =============================================================================
;; Debugging

(defn trace-lvar [a lvar]
  `(println (format "%5s = %s" (str '~lvar) (reify ~a ~lvar))))

(defmacro log [s]
  `(fn [a#]
     (println ~s)
     a#))

(defmacro trace-lvars [title & lvars]
  (let [a (gensym "a")]
    `(fn [~a]
       (println ~title)
       ~@(map (partial trace-lvar a) lvars)
       ~a)))

(defmacro trace-s []
  `(fn [a#]
     (println (str a#))
     a#))