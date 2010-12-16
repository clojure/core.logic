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
  clojure.lang.PersistentArrayMap$Seq
  clojure.lang.PersistentHashMap
  clojure.lang.MapEntry
  clojure.lang.PersistentHashSet
  clojure.lang.APersistentMap$KeySeq
  clojure.lang.PersistentHashMap$NodeSeq
  clojure.lang.LazySeq
  clojure.lang.ChunkedCons)

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

(defrecord Substitutions [s s']
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

(defmacro unit [a]
  `(list ~a))

(defn mplus
  "Like interleave but keeps going even after some seqs are exhausted."
  ([] nil)
  ([c1] c1)
  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (cond
         (and s1 s2) (cons (first s1)
                           (cons (first s2) 
                                 (mplus (rest s1) (rest s2))))
         s1 s1
         s2 s2))))
  ([c1 c2 & colls] 
     (lazy-seq 
      (let [ss (map seq (conj colls c2 c1))]
        (when-let [ss (remove nil? ss)]
          (concat (map first ss) (apply mplus (map rest ss))))))))

;; it maybe be a problem that this is not lazy
;; each time around we force a lazy seq
;; we might want to just convert this into a reduce
(defn bind
  ([a] (unit a))
  ([a & gs]
     (lazy-seq
      (let [a (if (seq? a) a (unit a))]
        (loop [a a g0 (first gs) g-rest (next gs)]
          (let [a' (seq (reduce concat (remove nil? (map g0 a))))] ;; #_(reduce concat (remove nil? (map g0 a)))
            (cond
             (nil? a') nil
             g-rest (recur a' (first g-rest) (next g-rest))
             :else a')))))))

(defn succeed [a] (unit a))

(defn fail [a] nil)

(def s* succeed)

(def u* fail)

(defmacro == [u v]
  `(fn [a#]
     (if-let [a'# (unify a# ~u ~v)]
       (unit a'#)
       nil)))

(defn bind-cond-e-clause [a]
  (fn [g-rest]
    `(bind ~a ~@g-rest)))

(defn bind-cond-e-clauses [a clauses]
  (map (bind-cond-e-clause a) clauses))

(defmacro cond-e [& clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (mplus ~@(bind-cond-e-clauses a clauses)))))

(defn lvar-bind [sym]
  ((juxt identity
         (fn [s] `(lvar '~s))) sym))

(defn lvar-binds [syms]
  (reduce concat (map lvar-bind syms)))

(defmacro exist [[& x-rest] & g-rest]
  `(fn [a#]
     (let [~@(lvar-binds x-rest)]
       (bind a# ~@g-rest))))

(defmacro run [& [n [x] & g-rest]]
  `(let [a# ((exist [~x] ~@g-rest
                    (fn [a'#]
                      (unit (reify a'# ~x))))
             empty-s)]
    (if ~n
      (take ~n a#) 
      a#)))

(defmacro run* [& body]
  `(run false ~@body))

(defn sym->lvar [sym]
  `(lvar '~sym))

(def *debug* (atom []))

(defmacro trace [a & lm]
  `(binding [*debug* (or ~a *debug*)]
     ~@lm))

(defn trace-lvar [a lvar]
  `(swap! *debug* conj (format "%5s = %s\n" (str '~lvar) (reify ~a ~lvar))))

(defmacro trace-lvars [title & lvars]
  (let [a (gensym "a")]
    `(fn [~a]
       (swap! *debug* conj (str ~title "\n"))
       ~@(map (partial trace-lvar a) lvars)
       (swap! *debug* conj "\n")
       (unit ~a))))

(defn print-debug [a]
  (println (reduce str @a)))

(defmacro trace-s []
  (let [a (gensym "a")]
   `(fn [~a]
      (swap! *debug* conj ~a)
      (unit ~a))))

(defmacro all
  ([] `s*)
  ([g] `(fn [a#] (~g a#)))
  ([g0 & g-rest] `(fn [a#] (bind a# ~g0 ~@g-rest))))

(deftype ThunkSeq [f]
  clojure.lang.Seqable
  (seq [this] (force f)))

(defmacro thunk-seq [e]
  `(ThunkSeq. (delay (lazy-seq ~e))))

(comment
  ; TODO : succeed is borked, probably fail as well

  (run* [q]
        (exist [x]
               (== x 5)
               (exist [y]
                      (== x y)
                      (== q x))))

  (run* [q]
        (== q 5))

  (run* [q]
        (== q 5)
        (== q 6))

  (run* [q]
        (cond-e
         ((== q 5))
         ((== q 6))))

  (run 1 [q]
       (cond-e
        ((== q 5))
        ((== q 6))))

  (run* [q]
        (== q [1 2]))

  (run* [q]
        (exist [x y]
               (== x 5)
               (== q x)))

  (run* [q]
        (exist [x y]
               (== y 5)
               (== x q)))

  (run* [q]
        (exist [x y]
               (== [x y] [1 5])
               (== q x)))
  
  (run* [q]
        (exist [x y]
               (== [x y] [1 5])
               (== [x y] q)))

  ;; minimum required for a stack overflow error
  (defn truth [q]
    (cond-e
     ((== q true))
     ((truth q))))

  (run 5 [q]
       (truth q))

  ;; hmm even w/ recursive functions we don't see this problem
  ;; (a x x x x)
  (letfn [(rec [x] (lazy-seq (cons 'x (rec x))))]
    (take 5
          (mplus (bind 'a (fn [x] (list x)))
                 (bind 'a rec))))
  
  ;; (a x)
  (letfn [(rec [x] (lazy-seq (cons 'x (rec x))))]
    (take 5
          (interleave (bind 'a (fn [x] (list x)))
                      (bind 'a rec))))

  ;; smoking gun
  (letfn [(rec [x] (mplus (bind 'a (fn [x] (list x)))
                          (bind 'a rec)))]
    (take 5
          (mplus (bind 'a (fn [x] (list x)))
                 (bind 'a rec))))

  ;; we still have a stack overflow error
  ;; even if we wrap the binds in rec with
  ;; lazy-seq still stack overflow
  (letfn [(rec [x] (interleave (bind 'a (fn [x] (list x)))
                               (bind 'a rec)))]
    (take 5
          (interleave (bind 'a (fn [x] (list x)))
                      (bind 'a rec))))

  ;; still a stack overflow
  ;; NOTE: this works if bind returns a lazy-seq
  ;; (a a) 
  (letfn [(rec [x] (interleave (bind 'a (fn [x] (list x)))
                               (lazy-seq (cons nil (bind 'a rec)))))]
    (take 5
          (interleave (bind 'a (fn [x] (list x)))
                      (bind 'a rec))))

  ;; this looks ok
  (letfn [(rec [x] (mplus (bind x (fn [x] (list (str x))))
                          (lazy-seq (cons x (bind (inc x) rec)))))]
    (take 20
          (mplus (bind 'a (fn [x] (list x)))
                 (bind 0 rec))))

  ;; works if we stick nil in there
  (letfn [(rec [x] (mplus (bind x (fn [x] (list (str x))))
                          (lazy-seq (cons nil (bind (inc x) rec)))))]
    (take 20
          (mplus (bind 'a (fn [x] (list x)))
                 (bind 0 rec))))

  ;; stackoverflow if we don't
  (letfn [(rec [x] (mplus (bind x (fn [x] (list (str x))))
                          (remove nil? (lazy-seq (cons nil (bind (inc x) rec))))))]
    (take 20
          (mplus (bind 'a (fn [x] (list x)))
                 (bind 0 rec))))

  ;; this works
  (letfn [(rec [x] (mplus (bind x (fn [x] (list (str x))))
                          (lazy-seq (cons nil (bind (inc x) rec)))))]
    (take 20
          (remove nil?
                  (mplus (bind 'a (fn [x] (list x)))
                         (bind 0 rec)))))

  (letfn [(rec [x] (mplus (thunk-seq (bind x (fn [x] (list (str x)))))
                          (thunk-seq (bind (inc x) rec))))]
    (take 20
          (mplus (bind 'a (fn [x] (list x)))
                 (thunk-seq (bind 0 rec)))))

  ;; doesn't work
  (letfn [(rec [x] (mplus (bind x (fn [x] (list (str x))))
                          (bind (inc x) rec)))]
    (take 20
          (mplus (bind 'a (fn [x] (list x)))
                 (bind 0 rec))))

  (letfn [(rec [x] (mplus (bind x (fn [x] (list (str x))))
                          (bind (inc x) rec)))]
    (take 20
          (mplus (bind 'a (fn [x] (list x)))
                 (bind 0 rec))))

  ;; we probably want to go back to our original code
  ;; and just return lazy seqs, mplus will only work
  ;; with pairs

  ;; this is a problem of *evaluation*
  ;; laziness would really make this simpler
 )