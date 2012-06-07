(ns cljs.core.logic
  (:refer-clojure :exclude [==])
  (:use-macros [cljs.core.logic.macros :only
                [defne defna defnu fresh == -inc]])
  (:require-macros [cljs.core.logic.macros :as m])
  (:require [clojure.set :as set]))

(def ^{:dynamic true} *occurs-check* true)

(defprotocol IUnifyTerms
  (-unify-terms [u v s]))

(defprotocol IUnifyWithNil
  (-unify-with-nil [v u s]))

(defprotocol IUnifyWithObject
  (-unify-with-object [v u s]))

(defprotocol IUnifyWithLVar
  (-unify-with-lvar [v u s]))

(defprotocol IUnifyWithLSeq
  (-unify-with-lseq [v u s]))

(defprotocol IUnifyWithSequential
  (-unify-with-seq [v u s]))

(defprotocol IUnifyWithMap
  (-unify-with-map [v u s]))

(defprotocol IUnifyWithSet
  (-unify-with-set [v u s]))

(defprotocol IReifyTerm
  (-reify-term [v s]))

(defprotocol IWalkTerm
  (-walk-term [v s]))

(defprotocol IOccursCheckTerm
  (-occurs-check-term [v x s]))

(defprotocol IBuildTerm
  (-build-term [u s]))

(defprotocol IBind
  (-bind [this g]))

(defprotocol IMPlus
  (-mplus [a f]))

(defprotocol ITake
  (-take* [a]))

;; =============================================================================
;; Pair

(defprotocol IPair
  (-lhs [this])
  (-rhs [this]))

(deftype Pair [lhs rhs]
  IEquiv
  (-equiv [this other]
    (and (= lhs (.-lhs other))
         (= rhs (.-rhs other))))
  ICounted
  (-count [_] 2)
  IIndexed
  (-nth [_ i] (condp = i
                   0 lhs
                   1 rhs
                   (throw (js/Error. "Index out of bounds"))))
  (-nth [_ i not-found] (condp = i
                             0 lhs
                             1 rhs
                             not-found))
  IPair
  (-lhs [_] lhs)
  (-rhs [_] rhs)
  IPrintable
  (-pr-seq [coll options]
    (list "(" (str lhs) " . " (str rhs) ")")))

(defn pair [lhs rhs]
  (Pair. lhs rhs))

;; =============================================================================
;; Substitutions

(defprotocol ISubstitutions
  (-occurs-check [this u v])
  (-ext [this u v])
  (-ext-no-check [this u v])
  (-walk [this v])
  (-walk* [this v])
  (-unify [this u v])
  (-reify-lvar-name [_])
  (-reify* [this v])
  (-reify [this v]))

(declare empty-s)
(declare choice)
(declare lvar)
(declare lvar?)
(declare pair)
(declare lcons)

(def not-found (js-obj))

(defn assq
  "Similar to Scheme assq, xs must be a List of Pairs"
  [k xs]
  (loop [xs (-seq xs)]
    (if (nil? xs)
      not-found
      (let [x (-first xs)
            lhs (.-lhs x)]
        (if (identical? k lhs)
          (.-rhs x)
          (recur (-next xs)))))))

(deftype Substitutions [s]
  IEquiv
  (-equiv [this o]
    (or (identical? this o)
        (and (instance? Substitutions o)
             (= s (.-s o)))))

  IPrintable
  (-pr-seq [this opts]
    (-pr-seq s opts))

  ISubstitutions
  (-occurs-check [this u v]
    (let [v (-walk this v)]
      (-occurs-check-term v u this)))
  
  (-ext [this u v]
    (if (and *occurs-check* (-occurs-check this u v))
      nil
      (-ext-no-check this u v)))

  (-ext-no-check [this u v]
    (Substitutions. (conj s (Pair. u v))))
  
  (-walk [this v]
    (cond
     (lvar? v) (let [rhs (assq v s)
                     vp (-walk this rhs)]
                (if (identical? not-found vp) v vp))
     :else v))
  
  (-walk* [this v]
    (let [v (-walk this v)]
      (-walk-term v this)))

  (-unify [this u v]
    (if (identical? u v)
      this
      (let [u (-walk this u)
            v (-walk this v)]
        (if (identical? u v)
          this
          (-unify-terms u v this)))))

  (-reify-lvar-name [this]
    (symbol (str "_." (count s))))

  (-reify* [this v]
    (let [v (-walk this v)]
      (-reify-term v this)))

  (-reify [this v]
    (let [v (-walk* this v)]
      (-walk* (-reify* empty-s v) v)))

  IBind
  (-bind [this g]
    (g this))

  IMPlus
  (-mplus [this f]
    (choice this f))

  ITake
  (-take* [this]
    this))

(defn make-s [s]
  (Substitutions. s))

(def empty-s (make-s '()))

(defn ^boolean subst? [x]
  (instance? Substitutions x))

(defn to-s [v]
  (let [s (reduce (fn [l [k v]]
                    (cons (pair k v) l))
                  () v)]
    (make-s s)))

;; =============================================================================
;; Logic Variables

(deftype LVar [name meta]
  Object
  (toString [this]
    (pr-str this))
  IHash
  (-hash [this]
    (-hash name))
  IMeta
  (-meta [this]
    meta)
  IWithMeta
  (-with-meta [this new-meta]
    (LVar. name meta))
  IPrintable
  (-pr-seq [_ opts]
    (list "<lvar:" (str name) ">"))
  IEquiv
  (-equiv [this o]
    (and (instance? LVar o)
         (let [o o]
           (identical? name (.-name o)))))
  IUnifyTerms
  (-unify-terms [u v s]
    (-unify-with-lvar v u s))
  IUnifyWithNil
  (-unify-with-nil [v u s]
    (-ext-no-check s v u))
  IUnifyWithObject
  (-unify-with-object [v u s]
    (-ext s v u))
  IUnifyWithLVar
  (-unify-with-lvar [v u s]
    (-ext-no-check s u v))
  IUnifyWithLSeq
  (-unify-with-lseq [v u s]
    (-ext s v u))
  IUnifyWithSequential
  (-unify-with-seq [v u s]
    (-ext s v u))
  IUnifyWithMap
  (-unify-with-map [v u s]
    (-ext s v u))
  IUnifyWithSet
  (-unify-with-set [v u s]
    (-ext s v u))
  IReifyTerm
  (-reify-term [v s]
    (-ext s v (-reify-lvar-name s)))
  IWalkTerm
  (-walk-term [v s] v)
  IOccursCheckTerm
  (-occurs-check-term [v x s]
    (= (-walk s v) x)))

(def lvar-sym-counter (atom 0))

(defn lvar
  ([] (lvar 'gen))
  ([name]
     (let [name (js* "~{} + '_' + ~{}"
                     (.substring name 2 (.-length name))
                     (swap! lvar-sym-counter inc))]
       (LVar. name nil))))

(defn ^boolean lvar? [x]
  (instance? LVar x))

;; =============================================================================
;; LCons

(defprotocol LConsSeq
  (-lfirst [this])
  (-lnext [this]))

(declare lcons?)

(defn lcons-pr-seq [x]
  (cond
   (lcons? x) (lazy-seq
               (cons (-lfirst x)
                     (lcons-pr-seq (-lnext x))))
   :else (list '. x)))

(deftype LCons [a d meta]
  IMeta
  (-meta [this]
    meta)
  IWithMeta
  (-withMeta [this new-meta]
    (LCons. a d new-meta))
  LConsSeq
  (-lfirst [_] a)
  (-lnext [_] d)
  IPrintable
  (-pr-seq [this opts]
    (pr-sequential pr-seq "(" " " ")" opts (lcons-pr-seq this)))
  IEquiv
  (-equiv [this o]
    (or (identical? this o)
        (and (instance? LCons o)
             (loop [me this
                    you o]
               (cond
                (nil? me) (nil? you)
                (lvar? me) true
                (lvar? you) true
                (and (lcons? me) (lcons? you))
                  (let [mef  (-lfirst me)
                        youf (-lfirst you)]
                    (and (or (= mef youf)
                             (lvar? mef)
                             (lvar? youf))
                         (recur (-lnext me) (-lnext you))))
                :else (= me you))))))
  IUnifyTerms
  (-unify-terms [u v s]
    (-unify-with-lseq v u s))
  IUnifyWithNil
  (-unify-with-nil [v u s] false)
  IUnifyWithObject
  (-unify-with-object [v u s] false)
  IUnifyWithLSeq
  (-unify-with-lseq [v u s]
    (loop [u u v v s s]
      (if (lvar? u)
        (-unify s u v)
        (cond
         (lvar? v) (-unify s v u)
         (and (lcons? u) (lcons? v))
           (if-let [s (-unify s (-lfirst u) (-lfirst v))]
             (recur (-lnext u) (-lnext v) s)
             false)
         :else (-unify s u v)))))
  IUnifyWithSequential
  (-unify-with-seq [v u s]
    (-unify-with-lseq u v s))
  IUnifyWithMap
  (-unify-with-map [v u s] false)
  IUnifyWithSet
  (-unify-with-set [v u s] false)
  IReifyTerm
  (-reify-term [v s]
    (loop [v v s s]
      (if (lcons? v)
        (recur (-lnext v) (-reify* s (-lfirst v)))
        (-reify* s v))))
  IWalkTerm
  (-walk-term [v s]
    (lcons (-walk* s (-lfirst v))
           (-walk* s (-lnext v))))
  IOccursCheckTerm
  (-occurs-check-term [v x s]
    (loop [v v x x s s]
      (if (lcons? v)
        (or (-occurs-check s x (-lfirst v))
            (recur (-lnext v) x s))
        (-occurs-check s x v)))))

(defn lcons
  "Constructs a sequence a with an improper tail d if d is a logic variable."
  [a d]
  (if (or (coll? d) (nil? d))
    (cons a (seq d))
    (LCons. a d nil)))

(defn ^boolean lcons? [x]
  (instance? LCons x))

;; =============================================================================
;; Unification

(extend-protocol IUnifyTerms
  nil
  (-unify-terms [u v s]
    (-unify-with-nil v u s))

  default
  (-unify-terms [u v s]
    (if (sequential? u)
      (-unify-with-seq v u s)
      (-unify-with-object v u s)))

  ObjMap
  (-unify-terms [u v s]
    (-unify-with-map v u s))
  
  PersistentArrayMap
  (-unify-terms [u v s]
    (-unify-with-map v u s))

  PersistentHashMap
  (-unify-terms [u v s]
    (-unify-with-map v u s))
  
  PersistentHashSet
  (-unify-terms [u v s]
    (-unify-with-set v u s)))

;; -----------------------------------------------------------------------------
;; Unify nil with X

(extend-protocol IUnifyWithNil
  nil
  (-unify-with-nil [v u s] s)

  default
  (-unify-with-nil [v u s] false))

;; -----------------------------------------------------------------------------
;; Unify Object with X

(extend-protocol IUnifyWithObject
  nil
  (-unify-with-object [v u s] false)

  default
  (-unify-with-object [v u s]
    (if (= u v) s false)))

;; -----------------------------------------------------------------------------
;; Unify LVar with X

(extend-protocol IUnifyWithLVar
  nil
  (-unify-with-lvar [v u s] (-ext-no-check s u v))

  default
  (-unify-with-lvar [v u s]
    (-ext s u v)))

;; -----------------------------------------------------------------------------
;; Unify LCons with X

(extend-protocol IUnifyWithLSeq
  nil
  (-unify-with-lseq [v u s] false)

  default
  (-unify-with-lseq [v u s]
    (if (sequential? v)
      (loop [u u v v s s]
        (if (seq v)
          (if (lcons? u)
            (if-let [s (-unify s (-lfirst u) (first v))]
              (recur (-lnext u) (next v) s)
              false)
            (-unify s u v))
          (if (lvar? u)
            (-unify s u '())
            false)))
      false)))

;; -----------------------------------------------------------------------------
;; Unify Sequential with X

(extend-protocol IUnifyWithSequential
  nil
  (-unify-with-seq [v u s] false)

  default
  (-unify-with-seq [v u s]
    (if (sequential? v)
      (loop [u u v v s s]
        (if (seq u)
          (if (seq v)
            (if-let [s (-unify s (first u) (first v))]
              (recur (next u) (next v) s)
              false)
            false)
          (if (seq v) false s)))
      false)))

;; -----------------------------------------------------------------------------
;; Unify IPersistentMap with X

(def not-found (js-obj))

(defn unify-with-map* [v u s]
  (if-not (cljs.core/== (count v) (count u))
    false
    (loop [ks (seq (keys u)) s s]
      (if ks
        (let [kf (first ks)
              vf (get v kf not-found)]
          (if (identical? vf not-found)
            false
            (if-let [s (-unify s (get u kf) vf)]
              (recur (next ks) s)
              false)))
        s))))

(extend-protocol IUnifyWithMap
  nil
  (-unify-with-map [v u s] false)

  default
  (-unify-with-map [v u s] false)

  ObjMap
  (-unify-with-map [v u s]
    (unify-with-map* v u s))

  PersistentArrayMap
  (-unify-with-map [v u s]
    (unify-with-map* v u s))

  PersistentHashMap
  (-unify-with-map [v u s]
    (unify-with-map* v u s)))

;; -----------------------------------------------------------------------------
;; Unify IPersistentSet with X

(extend-protocol IUnifyWithSet
  nil
  (-unify-with-set [v u s] false)

  default
  (-unify-with-set [v u s] false)

  PersistentHashSet
  (-unify-with-set [v u s]
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
                (-unify s (concat ulvars umissing)
                        (concat vmissing vlvars))))
            false)
          s)))))

;; =============================================================================
;; Reification

(extend-protocol IReifyTerm
  nil
  (-reify-term [v s] s)

  default
  (-reify-term [v s]
    (if (sequential? v)
      (loop [v v s s]
        (if (seq v)
          (recur (next v) (-reify* s (first v)))
          s))
      s)))

;; =============================================================================
;; Walk Term

(defn walk-term-map* [v s]
  (loop [v v r {}]
    (if (seq v)
      (let [[vfk vfv] (first v)]
        (recur (next v) (assoc r vfk (-walk* s vfv))))
      r)))

(extend-protocol IWalkTerm
  nil
  (-walk-term [v s] nil)

  default
  (-walk-term [v s]
    (if (sequential? v)
      (map #(-walk* s %) v)
      v))

  PersistentVector
  (-walk-term [v s]
    (loop [v v r []]
      (if (seq v)
        (recur (next v) (conj r (-walk* s (first v))))
        r)))

  ObjMap
  (-walk-term [v s] (walk-term-map* v s))

  PersistentHashMap
  (-walk-term [v s] (walk-term-map* v s))

  PersistentHashSet
  (-walk-term [v s]
    (loop [v v r {}]
      (if (seq v)
        (recur (next v) (conj r (-walk* s (first v))))
        r))))

;; =============================================================================
;; Occurs Check Term

(extend-protocol IOccursCheckTerm
  nil
  (-occurs-check-term [v x s] false)

  default
  (-occurs-check-term [v x s]
    (if (sequential? v)
      (loop [v v x x s s]
        (if (seq v)
          (or (-occurs-check s x (first v))
              (recur (next v) x s))
          false))
      false)))

;; =============================================================================
;; Goals and Goal Constructors

(extend-type default
  ITake
  (-take* [this] this))

;; TODO: Choice always holds a as a list, can we just remove that?

(declare Inc)

(deftype Choice [a f]
  IBind
  (-bind [this g]
    (-mplus (g a) (-inc (-bind f g))))
  IMPlus
  (-mplus [this fp]
    (Choice. a (-inc (-mplus (fp) f))))
  ITake
  (-take* [this]
    (lazy-seq (cons (first a) (lazy-seq (-take* f))))))

(defn choice [a f]
  (Choice. a f))

;; -----------------------------------------------------------------------------
;; MZero

(extend-protocol IBind
  nil
  (-bind [_ g] nil))

(extend-protocol IMPlus
  nil
  (-mplus [_ b] b))

(extend-protocol ITake
  nil
  (-take* [_] '()))

;; -----------------------------------------------------------------------------
;; Unit

(extend-type default
  IMPlus
  (-mplus [this f]
    (Choice. this f)))

;; -----------------------------------------------------------------------------
;; Inc

(deftype Inc [f]
  IFn
  (-invoke [_] (f))
  IBind
  (-bind [this g]
    (-inc (-bind (f) g)))
  IMPlus
  (-mplus [this fp]
    (-inc (-mplus (fp) this)))
  ITake
  (-take* [this] (lazy-seq (-take* (f)))))

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

;; =============================================================================
;; conda (soft-cut), condu (committed-choice)

(defprotocol IIfA
  (-ifa [b gs c]))

(defprotocol IIfU
  (-ifu [b gs c]))

(extend-protocol IIfA
  nil
  (-ifa [b gs c]
       (when c
         (force c))))

(extend-protocol IIfU
  nil
  (-ifu [b gs c]
       (when c
         (force c))))

(extend-type Substitutions
  IIfA
  (-ifa [b gs c]
       (loop [b b [g0 & gr] gs]
         (if g0
           (when-let [b (g0 b)]
             (recur b gr))
           b))))

(extend-type Substitutions
  IIfU
  (-ifu [b gs c]
       (loop [b b [g0 & gr] gs]
         (if g0
           (when-let [b (g0 b)]
             (recur b gr))
           b))))

(extend-type Inc
  IIfA
  (-ifa [b gs c]
       (-inc (-ifa (b) gs c)))
  IIfU
  (-ifu [b gs c]
       (-inc (-ifu (b) gs c))))

(extend-protocol IIfA
  Choice
  (-ifa [b gs c]
       (reduce -bind b gs)))

;; TODO: Choice always holds a as a list, can we just remove that?
(extend-protocol IIfU
  Choice
  (-ifu [b gs c]
       (reduce -bind (.-a b) gs)))

;; =============================================================================
;; Useful goals

(defn nilo
  "A relation where a is nil"
  [a]
  (m/== nil a))

(defn emptyo
  "A relation where a is the empty list"
  [a]
  (m/== '() a))

(defn conso
  "A relation where l is a collection, such that a is the first of l 
  and d is the rest of l"
  [a d l]
  (m/== (lcons a d) l))

(defn firsto
  "A relation where l is a collection, such that a is the first of l"
  [l a]
  (fresh [d]
    (conso a d l)))

(defn resto
  "A relation where l is a collection, such that d is the rest of l"
  [l d]
  (fresh [a]
    (m/== (lcons a d) l)))

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

;; TODO: change to lazy-seq
(defn prefix [s <s]
  (if (= s <s)
    ()
    (cons (first s) (prefix (rest s) <s))))