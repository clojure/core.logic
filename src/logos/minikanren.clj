(ns logos.minikanren
  (:refer-clojure :exclude [reify inc == take])
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

(deftype rest-lvarT [name s]
  Object
  (toString [this] (str "<lvar:" name ">")))

(defn ^rest-lvarT rest-lvar
  ([] (rest-lvarT. (gensym) nil))
  ([name] (rest-lvarT. name nil))
  ([name s] (rest-lvarT. name s)))

(defmethod print-method rest-lvarT [x writer]
  (.write writer (str "<lvar:" (.name ^rest-lvarT x) ">")))

(defn lvar? [x]
  (or (instance? lvarT x) (instance? rest-lvarT x)))

(defn rest-lvar? [x]
  (instance? rest-lvarT x))

(defn rest-lvar-sym? [x]
  (= (first (str x)) \&))

;; =============================================================================
;; Pairs

;; NOTE: this abstraction is only used for the goal and goal constructor portion
;; of the codebase

(defprotocol IPair
  (lhs [this])
  (rhs [this]))

(deftype pairT [lhs rhs]
  IPair
  (lhs [this] lhs)
  (rhs [this] rhs)
  clojure.lang.ISeq
  (first [this] lhs)
  (more [this] rhs)
  Object
  (toString [this] (str "(" lhs " . " rhs ")")))

(defn ^pairT pair [lhs rhs]
  (pairT. lhs rhs))

(defn pair? [x]
  (instance? pairT x))

(defmethod print-method pairT [x w]
  (.write w (str "(" (lhs x)  " . " (rhs x)  ")")))

;; =============================================================================
;; LCons

;; NOTE: The ISeq interface expects that next returns a Seq. LCons may or may
;; not be real seqs. For example, if the right hand side is a logic variable. So
;; that we can iterate over sequences that may contain logic variables at the
;; end and Clojure sequences we define LConSeq and extend all the core Clojure
;; datatypes to this Protocol.

(defprotocol LConsSeq
  (lfirst [this])
  (lnext [this]))

;; TODO: clean up the printing code

(defprotocol LConsPrint
  (toShortString [this]))

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
  ;; equiv
  ;; equals
  ;; hashCode
  )

(defmethod print-method LCons [x writer]
  (.write writer (str x)))

;; TODO: define lconj
;; reverse the arguments and we can avoid the conditional here
;; just dispatch on type
;; lconj also give us the freedom to create the correct type
;; during reification
;; lconj when given a logic var convert's to a sequence just
;; for unification. if the var gets assocaited with a data
;; structure of a particular type, during reification lconj
;; can reconstruct. Even maps!

(defn lcons [a d]
  (if (or (coll? d) (nil? d))
    (cons a (seq d))
    (LCons. a d)))

(defn lcons? [x]
  (instance? LCons x))

(defn extend-type-to-lcons-seq [type]
  `(clojure.core/extend-type ~type
     LConsSeq
     (~'lfirst [this#] (clojure.core/first this#))
     (~'lnext [this#] (clojure.core/next this#))))

(defmacro extend-to-lcons-seq [& types]
  `(do
     ~@(map extend-type-to-lcons-seq types)))

(extend-to-lcons-seq
  clojure.lang.Cons
  clojure.lang.PersistentList
  clojure.lang.PersistentVector
  clojure.lang.PersistentVector$ChunkedSeq
  clojure.lang.PersistentArrayMap
  clojure.lang.PersistentHashMap
  clojure.lang.MapEntry
  clojure.lang.PersistentHashSet
  clojure.lang.APersistentMap$KeySeq)

(defmacro llist
  ([f s] `(lcons ~f ~s))
  ([f s & rest] `(lcons ~f (llist ~s ~@rest))))

;; =============================================================================
;; Unification

(declare lookup)
(declare lookup*)
(declare ext-no-check)
(declare ext)
(declare unify)
(declare length)
(declare empty-s)

(defn print-identity [v]
  (println v) v)

;; TODO: convert to macro

(defn lcoll? [x]
  (or (lcons? x)
      (and (coll? x) (seq x))))

;; TODO: move identical? check before lookup

(defn unify*
  ([s u v] (unify* s u v false))
  ([s u v in-seq]
     (let [u (lookup s u)
           v (lookup s v)]
       (cond
        (identical? u v) s
        (lvar? u) (cond
                   (lvar? v) (ext-no-check s u v)
                   (and in-seq (nil? v)) (ext s u '())
                   :else (ext s u v))
        (lvar? v) (if (and in-seq (nil? u))
                    (ext s v '())
                    (ext s v u))
        (and (lcoll? u) (lcoll? v)) (let [uf (lfirst u)
                                          ur (lnext u)
                                          vf (lfirst v)
                                          vr (lnext v)]
                                      (let [s (unify* s uf vf)]
                                        (and s (unify* s ur vr true))))
        (= u v) s
        :else false))))

;; =============================================================================
;; Reification

;; OPTIMIZE: add interfaces to dispatch on the type of v ?
;; means we would need to reverse the arguments

;; TODO: replace lcons with lconj, we can then skip the
;; the seq-to-type coercions
;; NOTE: how do we avoid vectors being backwards?
;; we need to switch to a real recursion passing
;; the current value which we'll lconj onto
;; also might want to consider extending the core
;; types with lempty, which just returns the empty
;; node, when we understand the repercussions
;; we can consider the real empty which preserves
;; metadata
;; NOTE: if we're going to do that, we can probably
;; leverage transients here

(defn reify-lookup [s v]
  (let [v' (lookup s v)]
    (cond
     (lvar? v') v'
     (lcoll? v') (let [vseq (if (map? v') (reduce concat v') v')
                       vf (reify-lookup s (lfirst vseq))
                       vn (reify-lookup s (lnext vseq))
                       r (lcons vf vn)]
                   (cond
                    (vector? v') (vec r)
                    (map? v') (apply hash-map r)
                    (set? v') (set r)
                    :else r))
     :else v')))

(defn reify-lvar-name [s]
  (symbol (str "_." (length s))))

(defn -reify [s v]
  (let [v (lookup s v)]
    (cond
     (lvar? v) (ext s v (reify-lvar-name s))
     (lcoll? v) (-reify (-reify s (lfirst v)) (lnext v))
     :else s)))

(defn reify [s v]
  (let [v (reify-lookup s v)]
    (reify-lookup (-reify empty-s v) v)))

;; =============================================================================
;; Substitutions

(defn lookup* [s v]
  (loop [v v p (find s v) s s ov v]
    (if (nil? p)
      v
      (let [[v v'] p]
        (if (lvar? v')
          (recur v' (find s v') s ov)
          v')))))

(defprotocol ISubstitutions
  (length [this])
  (ext [this x v])
  (ext-no-check [this x v])
  (lookup [this v])
  (unify [this u v]))

;; TODO : add occurs-check, and add to ext
;; NOTE : consider the right-hand-side optimization

(defrecord Substitutions [s s']
  ISubstitutions
  (length [this] (count s'))
  (ext [this x v]
       (ext-no-check this x v))
  (ext-no-check [this x v]
                (Substitutions. (assoc s x v)
                                (conj s' (pair x v))))
  (lookup [this v]
          (lookup* s v))
  (unify [this u v] (unify* this u v)))

(def empty-s (Substitutions. {} []))

(defn to-s [v]
  (let [s (reduce (fn [m [k v]] (assoc m k v)) {} v)
        s' (vec (map (partial apply pair) v))]
    (Substitutions. s s')))

;; =============================================================================
;; Goals and Goal Constructors

(defprotocol IMPlus
  (mplus [this f]))

(defprotocol IBind
  (bind [this g]))

(defprotocol ITake
  (take* [this n f v]))

(deftype MZero []
  clojure.lang.IFn
  (invoke [this] this)
  IMPlus
  (mplus [this f] (f))
  IBind
  (bind [this g] this)
  ITake
  (take* [this n f v] v))

(def -mzero (MZero.))

(defmacro mzero []
  `-mzero)

(declare take)

(deftype Choice [a f']
  clojure.lang.IFn
  (invoke [this] this)
  IPair
  (lhs [this] a)
  (rhs [this] f')
  IMPlus
  (mplus [this f] (Choice. (f) f'))
  IBind
  (bind [this g] (mplus (g a) (fn [] (bind (f') g))))
  ITake
  (take* [this n f v] (take (and n (dec n)) f' (conj v (a)))))

(deftype Unit [a]
  clojure.lang.IFn
  (invoke [this] a)
  IMPlus
  (mplus [this f] (Choice. a f))
  IBind
  (bind [this g] (g a))
  ITake
  (take* [this n f v] (conj v a)))

(defmacro unit [a]
  `(Unit. ~a))

(deftype Inc [a]
  clojure.lang.IFn
  (invoke [this] (a))
  IMPlus
  (mplus [this f] (Inc. (fn [] (mplus (f) a))))
  IBind
  (bind [this g] (Inc. (fn [] (bind (this) g))))
  ITake
  (take* [this n f v] (take n f v)))

(defmacro inc [e]
  `(Inc. (fn [] ~e)))

(defmacro choice [a f]
  `(Choice. ~a ~f))

(defn succeed [a]
  (unit a))

(defn fail [a]
  (mzero))

(def s# succeed)

(def u# fail)

(defmacro == [u v]
  `(fn [a#]
     (if-let [s# (unify a# ~u ~v)]
       (unit s#)
       (mzero))))

(defmacro mplus*
  ([e] e)
  ([e0 & e-rest] `(mplus ~e0 (fn [] (mplus* ~@e-rest)))))

(defn bind-cond-e-clause [s]
  (fn [[g0 & g-rest]]
    `(bind* (~g0 ~s) ~@g-rest)))

(defn bind-cond-e-clauses [s clauses]
  (map (bind-cond-e-clause s) clauses))

(defmacro cond-e [& clauses]
  (let [a (gensym "a")]
   `(fn [~a]
      (inc
       (mplus* ~@(bind-cond-e-clauses a clauses))))))

(defn lvar-bind [sym]
  ((juxt identity
         (fn [s] (if (rest-lvar-sym? s)
                   `(rest-lvar '~s)
                   `(lvar '~s)))) sym))

(defn lvar-binds [syms]
  (reduce concat (map lvar-bind syms)))

;; TODO: put the substitution in the var
;; then we can do an identical? check to see if
;; we're looking at an unassociated var

(defmacro exist [[& x-rest] g0 & g-rest]
  `(fn [a#]
     (inc
      (let [~@(lvar-binds x-rest)]
        (bind* (~g0 a#) ~@g-rest)))))

(defmacro bind*
  ([e] e)
  ([e g0 & g-rest] `(bind* (bind ~e ~g0) ~@g-rest)))

(defmacro run [& [n [x] g0 & g-rest]]
  `(take ~n
         (fn []
           ((exist [~x] ~g0 ~@g-rest
                   (fn [a#]
                     (unit (reify a# ~x))))
            empty-s))))

(defn take
  ([n f] (take n f [])) 
  ([n f v]
     #_(println "take" n f v)
     (if (and n (zero? n))
       v
       (let [f' (f)]
         #_(println "f'" f')
         (take* f' n f' v)))))

(defmacro run* [& body]
  `(run false ~@body))

(defn sym->lvar [sym]
  (if (rest-lvar-sym? sym)
    `(rest-lvar '~sym)
    `(lvar '~sym)))

(defn trace-lvar [a lvar]
  `(println (format "%5s = %s" (str '~lvar) (reify ~a ~lvar))))

(defmacro trace-lvars [title & lvars]
  (let [a (gensym "a")]
   `(fn [~a]
      (println ~title)
      ~@(map (partial trace-lvar a) lvars)
      (println)
      (unit ~a))))

(defmacro trace-s []
  (let [a (gensym "a")]
   `(fn [~a]
      (println ~a)
      ~a)))