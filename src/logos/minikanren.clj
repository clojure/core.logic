(ns logos.minikanren
  (:refer-clojure :exclude [reify ==])
  (:use [clojure.pprint :only [pprint]]))

;; =============================================================================
;; Logic Variables

(deftype lvarT [name s]
  Object
  (toString [this] (str "<lvar:" name ">")))

(defn ^lvarT lvar
  ([] (lvarT. (gensym) nil))
  ([name] (lvarT. name nil))
  ([name s] (lvarT. name s)))

(defmethod print-method lvarT [x writer]
  (.write writer (str "<lvar:" (.name ^lvarT x) ">")))

(defn lvar? [x]
  (instance? lvarT x))

;; =============================================================================
;; LCons

;; NOTE: The ISeq interface expects that next returns a Seq. LCons may or may
;; not be real seqs. For example, if the right hand side is a logic variable. So
;; that we can iterate over sequences that may contain logic variables at the
;; end and Clojure sequences we define LConSeq and extend all the core Clojure
;; datatypes to this Protocol.

;; LConsSeq is not a general datastructure, it's only meant to be used within
;; miniKaren logic operations.

(defprotocol LConsSeq
  (lfirst [this])
  (lnext [this]))

;; TODO: clean up the printing code

(defprotocol LConsPrint
  (toShortString [this]))

(declare lcons?)

(deftype LCons [a d]
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
              (and (lcons? o)
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
            (loop [hash 1 xs this]
              (if (or (nil? xs) (lvar? xs))
                hash
                (let [val (lfirst xs)]
                 (recur (unchecked-add-int
                         (unchecked-multiply-int 31 hash)
                         (clojure.lang.Util/hash val))
                        (lnext xs)))))))

(defmethod print-method LCons [x writer]
  (.write writer (str x)))

(defn lcons [a d]
  (if (or (coll? d) (nil? d))
    (cons a (seq d))
    (LCons. a d)))

(defn lcons? [x]
  (instance? LCons x))

(extend-protocol LConsSeq
  clojure.lang.IPersistentCollection
  (lfirst [this] (clojure.core/first this))
  (lnext [this] (clojure.core/next this)))

(defmacro llist
  ([f s] `(lcons ~f ~s))
  ([f s & rest] `(lcons ~f (llist ~s ~@rest))))

;; TODO: convert to macro

(defn lcoll? [x]
  (or (lcons? x)
      (and (coll? x) (seq x))))

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
  (unify-seq [this u v in-seq])
  (reify-lvar-name [_])
  (-reify [this v])
  (reify [this v]))

(declare empty-s)

(deftype Substitutions [s s']
  ISubstitutions

  (length [this] (count s'))

  (occurs-check [this u v]
                (cond
                 (lvar? v) (= (walk this v) v)
                 (lcoll? v) (or (occurs-check this u (lfirst v))
                                (occurs-check this u (lnext v))))
                :else false)
  
  (ext [this u v]
       (if (occurs-check this u v)
         this
         (ext-no-check this u v)))

  (ext-no-check [this u v]
                (Substitutions. (assoc s u v)
                                (conj s' [u v])))

  (walk [this v]
          (loop [v v p (find s v) s s ov v]
            (if (nil? p)
              v
              (let [[v v'] p]
                (if (lvar? v')
                  (recur v' (find s v') s ov)
                  v')))))

  ;; TODO : revisit recur here. Main issue was how to reconstruct
  ;; types ?

  (walk* [this v]
         (let [v' (walk this v)]
           (cond
            (lvar? v') v'
            (lcoll? v') (let [vseq (if (map? v') (reduce concat v') v')
                              vf (walk* this (lfirst vseq))
                              vn (walk* this (lnext vseq))
                              r (lcons vf vn)]
                          (cond
                           (vector? v') (vec r)
                           (map? v') (apply hash-map r)
                           (set? v') (set r)
                           :else r))
            :else v')))

  (unify [this u v] (unify-seq this u v false))

  ;; TODO : for sequences this unnecessarily stack-consuming
  ;; as well as checking for conditions that don't matter
  ;; for sequences

  (unify-seq [this u v in-seq]
             (if (identical? u v)
               this
               (let [u (walk this u)
                     v (walk this v)]
                 (cond
                  (identical? u v) this
                  (lvar? u) (cond
                             (lvar? v) (ext-no-check this u v)
                             (and in-seq (nil? v)) (ext this u '())
                             :else (ext this u v))
                  (lvar? v) (if (and in-seq (nil? u))
                              (ext this v '())
                              (ext this v u))
                  (and (lcoll? u) (lcoll? v)) (let [uf (lfirst u)
                                                    ur (lnext u)
                                                    vf (lfirst v)
                                                    vr (lnext v)]
                                                (let [s (unify this uf vf)]
                                                  (and s (unify-seq s ur vr true))))
                  (= u v) this
                  :else false))))

  (reify-lvar-name [this]
                   (symbol (str "_." (length this))))

  ;; TODO : unnecessarily stack consuming

  (-reify [this v]
          (let [v (walk this v)]
            (cond
             (lvar? v) (ext this v (reify-lvar-name this))
             (lcoll? v) (-reify (-reify this (lfirst v)) (lnext v))
             :else this)))

  (reify [this v]
         (let [v (walk* this v)]
           (walk* (-reify empty-s v) v))))

(def empty-s (Substitutions. {} []))

(defn subst? [x]
  (instance? Substitutions x))

(defn to-s [v]
  (let [s (reduce (fn [m [k v]] (assoc m k v)) {} v)
        s' (vec (map (partial apply vector) v))]
    (Substitutions. s s')))

;; =============================================================================
;; Goals and Goal Constructors

(defprotocol IBind
  (bind [this g]))

(defprotocol IMPlus
  (mplus [a b]))

;; MZero
(extend-protocol IBind
  nil
  (bind [_ g] nil))

(extend-protocol IMPlus
  nil
  (mplus [_ b] b))

;; Unit
(extend-type Substitutions
  IBind
  (bind [this g]
        (g this))
  IMPlus
  (mplus [this b]
         (cond
          (nil? b) this
          (subst? b) (cons this
                           (cons b nil))
          :else (cons this b))))

(defmacro mplus*
  ([e] e)
  ([e & e-rest]
     `(mplus ~e (lazy-seq (let [r# (mplus* ~@e-rest)]
                            (cond
                             (subst? r#) (cons r# nil)
                             :else r#))))))

;; Streams

(defn extend-seq-type-to-bind-and-mplus [type]
  `(extend-type ~type
     IBind
     (~'bind [this# g#]
           (if-let [r# (seq (map g# this#))]
             (reduce mplus r#)))
     IMPlus
     (~'mplus [this# b#]
            (cond
             (nil? b#) this#
             (subst? b#) (cons b# this#)
             :else (cons (first this#)
                         (cons (first b#)
                               (mplus* (next b#) (next this#))))))))

(defmacro extend-seq-types-to-bind-and-mplus [& types]
  `(do
     ~@(map extend-seq-type-to-bind-and-mplus types)))

(extend-seq-types-to-bind-and-mplus
  clojure.lang.LazySeq
  clojure.lang.PersistentList
  clojure.lang.Cons)

(defn succeed [a] a)

(defn fail [a] nil)

(def s* succeed)

(def u* fail)

(defmacro bind*
  ([a g] `(bind ~a ~g))
  ([a g & g-rest]
     `(bind* (bind ~a ~g) ~@g-rest)))

(defmacro == [u v]
  `(fn [a#]
     (if-let [a'# (unify a# ~u ~v)]
       a'#
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
        (mplus* ~@(bind-cond-e-clauses a clauses))))))

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
   :else (list x)))

(defmacro run [& [n [x] & g-rest]]
  `(let [r# (let [~x (lvar '~x)]
              (->> (to-seq ((fn [a#] (bind* a# ~@g-rest)) empty-s))
                   (remove nil?)
                   (map (reifier ~x))))]
    (if ~n (take ~n r#) r#)))

(defmacro run* [& body]
  `(run false ~@body))

(defn sym->lvar [sym]
  `(lvar '~sym))

(defmacro all
  ([] `s*)
  ([& g-rest] `(fn [a#] (bind* a# ~@g-rest))))

;; =============================================================================
;; Debugging

(def *debug* (atom []))

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