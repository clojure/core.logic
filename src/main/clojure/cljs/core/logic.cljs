(ns cljs.core.logic
  (:refer-clojure :exclude [==])
  (:require-macros [cljs.core.logic :as m
                    :refer [defne defna defnu fresh == -inc]])
  (:require [clojure.set :as set]
            [clojure.walk :refer [postwalk]]))

(def ^:dynamic *logic-dbs* [])

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

(deftype Pair [lhs rhs]
  IEquiv
  (-equiv [this other]
    (and (= lhs (.-lhs other))
         (= rhs (.-rhs other))))
  ICounted
  (-count [_] 2)
  IIndexed
  (-nth [_ i]
    (case i
      0 lhs
      1 rhs
      (throw (js/Error. "Index out of bounds"))))
  (-nth [_ i not-found]
    (case i
      0 lhs
      1 rhs
      not-found))
  IPrintWithWriter
  (-pr-writer [coll writer opts]
    (-write writer (str "(" lhs " . " rhs ")"))))

(defn pair [lhs rhs]
  (Pair. lhs rhs))

;; =============================================================================
;; Substitutions

(declare LVar)

(defn ^boolean lvar? [x]
  (instance? LVar x))

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

(declare empty-s choice lvar lvar? lcons fail)

(def not-found (js-obj))

(defn assq
  "Similar to Scheme assq, xs must be a List of Pairs"
  [k ^not-native xs]
  (loop [^not-native xs (-seq xs)]
    (if (nil? xs)
      not-found
      (let [x   (-first xs)
            lhs (.-lhs x)]
        (if (identical? k lhs)
          (.-rhs x)
          (recur (-next xs)))))))

(deftype Substitutions [s c _meta]
  IMeta
  (-meta [_] _meta)

  IWithMeta
  (-with-meta [_ new-meta]
    (Substitutions. s c new-meta))

  IEquiv
  (-equiv [this o]
    (or (identical? this o)
        (and (instance? Substitutions o)
             (= s (.-s o)))))

  IPrintWithWriter
  (-pr-writer [this writer opts]
    (-pr-writer s writer opts))

  ISubstitutions
  (-occurs-check [this u v]
    (let [v (-walk this v)]
      (-occurs-check-term v u this)))
  
  (-ext [this u v]
    (if (and ^boolean (:occurs-check _meta)
             ^boolean (-occurs-check this u v))
      (fail this)
      (-ext-no-check this u v)))

  (-ext-no-check [this u v]
    (Substitutions. (conj s (Pair. u v)) c _meta))
  
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
      (-walk* ^not-native (-reify* ^not-native empty-s v) v)))

  IBind
  (-bind [this g]
    (g this))

  IMPlus
  (-mplus [this f]
    (choice this f))

  ITake
  (-take* [this]
    this))

(defn make-s
  ([s]
     (Substitutions. s () nil))
  ([s c]
     (Substitutions. s c nil)))

(def ^not-native empty-s (make-s '() nil))

(defn ^boolean subst? [x]
  (instance? Substitutions x))

(defn to-s [v]
  (let [s (reduce (fn [l [k v]]
                    (conj l (pair k v)))
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
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer (str "<lvar:" name ">")))
  IEquiv
  (-equiv [this o]
    (and (instance? LVar o)
         (let [o o]
           (identical? name (.-name o)))))
  IUnifyTerms
  (-unify-terms [u v s]
    (-unify-with-lvar v u s))
  IUnifyWithNil
  (-unify-with-nil [v u ^not-native s]
    (-ext-no-check s v u))
  IUnifyWithObject
  (-unify-with-object [v u ^not-native s]
    (-ext s v u))
  IUnifyWithLVar
  (-unify-with-lvar [v u ^not-native s]
    (-ext-no-check s u v))
  IUnifyWithLSeq
  (-unify-with-lseq [v u ^not-native s]
    (-ext s v u))
  IUnifyWithSequential
  (-unify-with-seq [v u ^not-native s]
    (-ext s v u))
  IUnifyWithMap
  (-unify-with-map [v u ^not-native s]
    (-ext s v u))
  IReifyTerm
  (-reify-term [v ^not-native s]
    (-ext s v (-reify-lvar-name s)))
  IWalkTerm
  (-walk-term [v s] v)
  IOccursCheckTerm
  (-occurs-check-term [v x ^not-native s]
    (= (-walk s v) x)))

(def lvar-sym-counter (atom 0))

(defn lvar
  ([] (lvar 'gen))
  ([name]
     (let [name (str name "_" (swap! lvar-sym-counter inc))]
       (LVar. name nil))))

;; =============================================================================
;; LCons

(defprotocol LConsSeq
  (-lfirst [this])
  (-lnext [this]))

(declare LCons Fail)

(defn ^boolean failed? [x]
  (instance? Fail x))

(defn ^boolean lcons? [x]
  (instance? LCons x))

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
  (-with-meta [this new-meta]
    (LCons. a d new-meta))
  LConsSeq
  (-lfirst [_] a)
  (-lnext [_] d)
  IPrintWithWriter
  (-pr-writer [this writer opts]
    (pr-sequential-writer writer pr-writer "(" " " ")" opts (lcons-pr-seq this)))
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
  (-unify-with-nil [v u s] (fail s))
  IUnifyWithObject
  (-unify-with-object [v u s] (fail s))
  IUnifyWithLSeq
  (-unify-with-lseq [^not-native v ^not-native u ^not-native s]
    (loop [u u v v s s]
      (if (lvar? u)
        (-unify s u v)
        (cond
         (lvar? v) (-unify s v u)

         (and (lcons? u) (lcons? v))
         (let [s (-unify s (-lfirst u) (-lfirst v))]
           (if-not (failed? s)
             (recur (-lnext u) (-lnext v) s)
             s))
         
         :else (-unify s u v)))))
  IUnifyWithSequential
  (-unify-with-seq [v u s]
    (-unify-with-lseq u v s))
  IUnifyWithMap
  (-unify-with-map [v u s] (fail s))
  IReifyTerm
  (-reify-term [v s]
    (loop [v v s s]
      (if (lcons? v)
        (recur (-lnext v) (-reify* s (-lfirst v)))
        (-reify* s v))))
  IWalkTerm
  (-walk-term [v ^not-native s]
    (lcons (-walk* s (-lfirst v))
           (-walk* s (-lnext v))))
  IOccursCheckTerm
  (-occurs-check-term [v x ^not-native s]
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
  
  PersistentArrayMap
  (-unify-terms [u v s]
    (-unify-with-map v u s))

  PersistentHashMap
  (-unify-terms [u v s]
    (-unify-with-map v u s)))

;; -----------------------------------------------------------------------------
;; Unify nil with X

(extend-protocol IUnifyWithNil
  nil
  (-unify-with-nil [v u s] s)

  default
  (-unify-with-nil [v u s] (fail s)))

;; -----------------------------------------------------------------------------
;; Unify Object with X

(extend-protocol IUnifyWithObject
  nil
  (-unify-with-object [v u s] (fail s))

  default
  (-unify-with-object [v u s]
    (if (= u v) s (fail s))))

;; -----------------------------------------------------------------------------
;; Unify LVar with X

(extend-protocol IUnifyWithLVar
  nil
  (-unify-with-lvar [v u ^not-native s] (-ext-no-check s u v))

  default
  (-unify-with-lvar [v u ^not-native s]
    (-ext s u v)))

;; -----------------------------------------------------------------------------
;; Unify LCons with X

(extend-protocol IUnifyWithLSeq
  nil
  (-unify-with-lseq [v u s] (fail s))

  default
  (-unify-with-lseq [v ^not-native u ^not-native s]
    (if (and (sequential? v) (not (nil? v)))
      (loop [u u ^not-native v (-seq v) s s]
        (if-not (nil? v)
          (if (lcons? u)
            (let [s (-unify s (-lfirst u) (-first v))]
              (if-not (failed? s)
                (recur (-lnext u) (-next v) s)
                s))
            (-unify s u v))
          (if (lvar? u)
            (-unify s u '())
            (fail s))))
      (fail s))))

;; -----------------------------------------------------------------------------
;; Unify Sequential with X

(extend-protocol IUnifyWithSequential
  nil
  (-unify-with-seq [v u s] (fail s))

  default
  (-unify-with-seq [^not-native v ^not-native u ^not-native s]
    (if (and (sequential? v) (not (nil? v)))
      (loop [^not-native u (-seq u) ^not-native v (-seq v) s s]
        (if-not (nil? u)
          (if-not (nil? v)
            (let [s (-unify s (-first u) (-first v))]
              (if-not (failed? s)
                (recur (-next u) (-next v) s)
                s))
            (fail s))
          (if-not (nil? v) (fail s) s)))
      (fail s))))

;; -----------------------------------------------------------------------------
;; Unify IPersistentMap with X

(def not-found (js-obj))

(defn unify-with-map* [v u s]
  (if-not (cljs.core/== (count v) (count u))
    (fail s)
    (loop [ks (seq (keys u)) s s]
      (if ks
        (let [kf (first ks)
              vf (get v kf not-found)]
          (if (identical? vf not-found)
            (fail s)
            (let [s (-unify s (get u kf) vf)]
              (if-not (failed? s)
                (recur (next ks) s)
                (fail s)))))
        s))))

(extend-protocol IUnifyWithMap
  nil
  (-unify-with-map [v u s] (fail s))

  default
  (-unify-with-map [v u s] (fail s))

  PersistentArrayMap
  (-unify-with-map [v u s]
    (unify-with-map* v u s))

  PersistentHashMap
  (-unify-with-map [v u s]
    (unify-with-map* v u s)))

;; =============================================================================
;; Reification

(extend-protocol IReifyTerm
  nil
  (-reify-term [v s] s)

  default
  (-reify-term [v ^not-native s]
    (if (sequential? v)
      (loop [v v s s]
        (if (seq v)
          (recur (next v) (-reify* s (first v)))
          s))
      s)))

;; =============================================================================
;; Walk Term

(defn walk-term-map* [^not-native v ^not-native s]
  (loop [^not-native v (-seq v) ^not-native r (transient {})]
    (if-not (nil? v)
      (let [[vfk vfv] (-first v)]
        (recur (-next v) (-assoc! r vfk (-walk* s vfv))))
      (persistent! r))))

(extend-protocol IWalkTerm
  nil
  (-walk-term [v s] nil)

  default
  (-walk-term [v ^not-native s]
    (if (sequential? v)
      (map #(-walk* s %) v)
      v))

  PersistentVector
  (-walk-term [^not-native v ^not-native s]
    (loop [^not-native v (-seq v) ^not-native r (transient [])]
      (if-not (nil? v)
        (recur (-next v) (-conj! r (-walk* s (first v))))
        (persistent! r))))

  PersistentHashMap
  (-walk-term [v s] (walk-term-map* v s)))

;; =============================================================================
;; Occurs Check Term

(extend-protocol IOccursCheckTerm
  nil
  (-occurs-check-term [v x s] false)

  default
  (-occurs-check-term [v x ^not-native s]
    (if (sequential? v)
      (loop [^not-native v (seq v) x x s s]
        (if-not (nil? v)
          (or (-occurs-check s x (-first v))
              (recur (-next v) x s))
          false))
      false)))

;; =============================================================================
;; Goals and Goal Constructors

(declare Choice)

(defn mplus [a f]
  (if (implements? IMPlus a)
    (-mplus ^not-native a f)
    (Choice. a f)))

(defn take* [x]
  (if (implements? ITake x)
    (-take* ^not-native x)
    (list x)))

(declare Inc)

(deftype Choice [a f]
  IBind
  (-bind [this g]
    (mplus (g a) (-inc (-bind ^not-native f g))))
  IMPlus
  (-mplus [this fp]
    (Choice. a (-inc (mplus (fp) f))))
  ITake
  (-take* [this]
    (lazy-seq (cons a (lazy-seq (take* f))))))

(defn choice [a f]
  (Choice. a f))

;; -----------------------------------------------------------------------------
;; Inc

(deftype Inc [f]
  IFn
  (-invoke [_] (f))
  IBind
  (-bind [this g]
    (-inc
      (let [^not-native a (f)]
        (-bind a g))))
  IMPlus
  (-mplus [this fp]
    (-inc (mplus (fp) this)))
  ITake
  (-take* [this] (lazy-seq (take* (f)))))

;; -----------------------------------------------------------------------------
;; Fail

(deftype Fail [a]
  IBind
  (-bind [this g] this)
  IMPlus
  (-mplus [this fp] fp)
  ITake
  (-take* [this] ()))

;; =============================================================================
;; Syntax

(defn succeed
  "A goal that always succeeds."
  [a] a)

(defn fail
  "A goal that always fails."
  [a] (Fail. a))

(def s# succeed)

(def u# fail)

;; =============================================================================
;; conda (soft-cut), condu (committed-choice)

(defprotocol IIfA
  (-ifa [b gs c]))

(defprotocol IIfU
  (-ifu [b gs c]))

(extend-protocol IIfA
  Fail
  (-ifa [b gs c]
       (when c
         (force c))))

(extend-protocol IIfU
  Fail
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
    (conj (prefix (rest s) <s) (first s))))

;; ==============================================================================
;; partial-maps

(defprotocol IUnifyWithPMap
  (unify-with-pmap [pmap u s]))

(defrecord PMap []
  IUnifyWithMap
  (-unify-with-map [v u s]
    (loop [ks (keys v) s s]
      (if (seq ks)
        (let [kf (first ks)
              uf (get u kf ::not-found)]
          (if (= uf ::not-found)
            (fail s)
            (let [s (-unify s (get v kf) uf)]
              (if-not (failed? s)
                (recur (next ks) s)
                s))))
        s)))

  IUnifyWithPMap
  (unify-with-pmap [v u s]
    (-unify-with-map v u s))

  IUnifyTerms
  (-unify-terms [u v s]
    (unify-with-pmap v u s))

  IUnifyWithLVar
  (-unify-with-lvar [v u s]
    (-ext-no-check s u v))

  IWalkTerm
  (-walk-term [v s]
    (walk-term-map* v s)))

(extend-protocol IUnifyWithPMap
  nil
  (unify-with-pmap [v u s] (fail s))

  default
  (unify-with-pmap [v u s] (fail s))

  cljs.core.logic.LVar
  (unify-with-pmap [v u s]
    (-ext s v u))

  PersistentArrayMap
  (unify-with-pmap [v u s]
    (-unify-with-map u v s))

  PersistentHashMap
  (unify-with-pmap [v u s]
    (-unify-with-map u v s)))

(defn partial-map
  "Given map m, returns partial map that unifies with maps even if it doesn't share all of the keys of that map.
   Only the keys of the partial map will be unified:

   (m/run* [q]
         (m/fresh [pm x]
                (m/== pm (partial-map {:a x}))
                (m/== pm {:a 1 :b 2})
                (m/== pm q)))
   ;;=> ({:a 1})"
  [m]
  (map->PMap m))

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

(defn unify [s u v]
  (if (identical? u v)
    s
    (let [u (-walk s u)
          v (-walk s v)]
      (if (identical? u v)
        s
        (-unify-terms u v s)))))

(defn unifier*
  "Unify the terms u and w."
  ([u w]
     (first
      (m/run* [q]
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
       (when-not (failed? s)
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

(defn to-stream [aseq]
  (let [aseq (drop-while nil? aseq)]
    (if (seq aseq)
      (choice (first aseq)
        (-inc (to-stream (next aseq))))
      (fail empty-s))))
