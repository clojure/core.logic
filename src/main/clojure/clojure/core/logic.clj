(ns clojure.core.logic
  (:refer-clojure :exclude [==])
  (:require [clojure.set :as set]
            [clojure.string :as string])
  (:import [java.io Writer]
           [java.util UUID]))

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

(defn record? [x]
  (instance? clojure.lang.IRecord x))

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
  (-with-id [this id]))

(defprotocol IConstraintId
  (-id [this]))

(defn id [c]
  (if (instance? clojure.core.logic.IConstraintId c)
    (-id c)
    (-> c meta ::id)))

(defn with-id [c id]
  (if (instance? clojure.core.logic.IWithConstraintId c)
    (-with-id c id)
    (vary-meta c assoc ::id id)))

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

(definterface IEnforceableConstraint)

(defn enforceable? [x]
  (instance? clojure.core.logic.IEnforceableConstraint x))

(defprotocol IUnwrapConstraint
  (unwrap [c]))

(defprotocol IMergeDomains
  (-merge-doms [a b]))

(defprotocol IMemberCount
  (member-count [this]))

(defprotocol IForceAnswerTerm
  (-force-ans [v x]))

;; -----------------------------------------------------------------------------
;; Tree Constraints

(defprotocol IDisunifyTerms
  (disunify-terms [u v s cs]))

(definterface ITreeConstraint)

(defn tree-constraint? [x]
  (instance? clojure.core.logic.ITreeConstraint x))

(defprotocol IPrefix
  (prefix [this]))

(defprotocol IWithPrefix
  (with-prefix [this p]))

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

(declare lvar? bindable? add-var)

(defn var-rands [a c]
  (->> (rands c)
    (map #(root-var a %))
    (filter lvar?)
    (into [])))

(defn unbound-rands [a c]
  (->> (var-rands a c)
    (filter #(lvar? (root-val a %)))))

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
       (walk* (-reify* (with-meta empty-s (meta s)) v) v)))
  ([s v r]
     (let [v (walk* s v)]
       (walk* (-reify* r v) v))))

(defn build [s u]
  (build-term u s))

(declare empty-s make-s)

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
    (contains? #{:s :vs :cs :cq :cqs :oc} k))
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
  ([] (make-s {}))
  ([m] (make-s m (make-cs)))
  ([m cs] (Substitutions. m nil nil cs nil #{} true nil)))

(defn tabled-s
  ([] (tabled-s false))
  ([oc] (tabled-s oc nil))
  ([oc meta]
     (-> (with-meta (make-s) meta)
       (assoc :oc oc)
       (assoc :ts (atom {})))))

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
  IVar
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
    (if (-> s clojure.core/meta :reify-vars)
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
  ITreeTerm
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

(defn unify-with-sequential* [u v s]
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
    (unify-with-sequential* u v s))

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
    (if (record? v)
      (walk-record-term v f)
      (with-meta
        (loop [v v r (transient {})]
          (if (seq v)
            (let [[vfk vfv] (first v)]
              (recur (next v) (assoc! r (walk-term (f vfk) f)
                                      (walk-term (f vfv) f))))
            (persistent! r)))
        (meta v))))

  clojure.lang.IRecord
  (walk-term [v f]
    (walk-record-term v f)))

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

(defmacro -run [opts [x :as bindings] & goals]
  (if (> (count bindings) 1)
    (let [[rbindings as-key [as]] (partition-by #{:as} bindings)]
      (if (seq as-key)
        `(-run ~opts [~as] (fresh [~@rbindings] (== ~as [~@rbindings]) ~@goals))
        `(-run ~opts [q#] (fresh ~bindings (== q# ~bindings) ~@goals))))
    `(let [opts# ~opts
           xs# (take* (fn []
                        ((fresh [~x]
                           ~@goals
                           (reifyg ~x))
                         (tabled-s (:occurs-check opts#)
                            (merge {:reify-vars true} opts#)))))]
       (if-let [n# (:n opts#)]
         (take n# xs#)
         xs#))))

(defmacro run
  "Executes goals until a maximum of n results are found."
  [n bindings & goals]
  `(-run {:occurs-check true :n ~n} ~bindings ~@goals))

(defmacro run*
  "Executes goals until results are exhausted."
  [bindings & goals]
  `(-run {:occurs-check true :n false} ~bindings ~@goals))

(defmacro run-nc
  "Executes goals until a maximum of n results are found. Does not 
   occurs-check."
  [n bindings & goals]
  `(-run {:occurs-check false :n ~n} ~bindings ~@goals))

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
                    (with-meta empty-s {:reify-vars false}) (-> u meta ::when))]
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
           s (unify (with-meta empty-s {:reify-vars false}) u w)]
       (when s
         (->> lvars
           (filter (fn [[name var]] (not= (walk s var) var)))   
           (map (fn [[name var]] [name (-reify s var)]))
           (into {})))))
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

;; TODO: remove v argument - David

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
  (fn [a]
    (let [coll (walk a coll)]
      (((fn everyg* [g coll]
          (if (seq coll)
            (all
             (g (first coll))
             (everyg* g (next coll)))
            s#)) g coll) a))))

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
;;
;; See - Claire Alvis, Dan Friedman, Will Byrd, et al
;; "cKanren - miniKanren with Constraints"
;; http://www.schemeworkshop.org/2011/papers/Alvis2011.pdf
;; http://github.com/calvis/cKanren

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

(defn irelevant? [c]
  (instance? clojure.core.logic.IRelevant c))

(defn relevant? [c a]
  (let [id (id c)]
    (and (or ((-> a :cs :cm) id)
             (nil? id))
         (-relevant? c a))))

(defn run-constraint [c]
  (fn [a]
    (if (or (not (irelevant? c)) (relevant? c a))
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
                              (nil? (get-dom a x ::fd))))
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
           r (-reify* (with-meta empty-s (meta a)) v)]
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
          (if (and (irelevant? c) (relevant? c a))
            ((addcg c) a)
            a))
        ((addcg c) a)))
    IUnwrapConstraint
    (unwrap [_] c)))

;; TODO: this stuff needs to be moved into fd - David

(defn get-dom-fd
  [a x]
  (if (lvar? x)
    (get-dom a x ::fd)
    x))

(defmacro let-dom
  [a vars & body]
  (let [get-var-dom (fn [a [v b]]
                      `(~b (let [v# (walk ~a ~v)]
                             (if (lvar? v#)
                               (get-dom-fd ~a v#)
                               v#))))]
   `(let [~@(mapcat (partial get-var-dom a) (partition 2 vars))]
      ~@body)))

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

  LCons
  (-force-ans [v x]
    (letfn [(loop [ys]
              (all
               (force-ans (lfirst ys))
               (if (lcons? (lnext ys))
                 (loop (lnext ys))
                 s#)))]
      (loop v))))

(defn force-ans [x]
  (fn [a]
    ((let [v (walk a x)]
       (if (lvar? v)
         (-force-ans (get-dom-fd a x) v)
         (let [x (root-var a x)]
           (if (sequential? v)
             (-force-ans (sort-by-strategy v x a) x)
             (-force-ans v x))))) a)))

(defn distribute [v* strategy]
  (fn [a]
    (add-attr a v* ::strategy ::ff)))

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

  LVar
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

(defn recover-vars-from-term [x]
  (let [r (atom #{})]
    (walk-term x
      (fn [x]
        (if (lvar? x)
          (do (swap! r conj x) x)
          x)))
    @r))

(defn recover-vars [p]
  (loop [p (seq p) r #{}]
    (if p
      (let [[u v] (first p)]
        (recur (next p)
          (clojure.set/union
            r (recover-vars-from-term u) (recover-vars-from-term v))))
      r)))

(declare normalize-store ground-term?)

(defn !=c
  [p]
  (reify
    ITreeConstraint
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
                      (nil? (unify a xv vv)) nil
                      :else (recur (next sp) (assoc (dissoc p x) xv vv))))
                  p))]
        (if p
          (when-not (empty? p)
            #_((normalize-store (with-prefix this p)) a)
            (bind* a (remcg this) (cgoal (!=c p))))
          ((remcg this) a))))
    IPrefix
    (prefix [_] p)
    IWithPrefix
    (with-prefix [_ p] (!=c p))
    IReifiableConstraint
    (reifyc [this v r a]
      (let [p* (-reify a (map (fn [[lhs rhs]] `(~lhs ~rhs)) p) r)]
        (if (empty? p*)
          '()
          `(~'!= ~@p*))))
    IConstraintOp
    (rator [_] `!=)
    (rands [_] (seq (recover-vars p)))
    IRunnable
    (runnable? [this s]
      (some #(not= (walk s %) %) (recover-vars p)))
    IRelevant
    (-relevant? [this s]
      (not (empty? p)))
    IConstraintWatchedStores
    (watched-stores [this] #{::subst})))

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
            (if  (some (fn [[u v]] (nil? (unify a u v))) p)
              a
              ((cgoal (!=c p)) a))))
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
  (-uninitialized [_] (PMap.)))

(defn partial-map
  "Given map m, returns partial map that unifies with maps even if it
   doesn't share all of the keys of that map."
  [m]
  (map->PMap m))

(defn partial-map? [x]
  (instance? PMap x))

(defn -featurec
  [x fs]
  (reify
    clojure.lang.IFn
    (invoke [this a]
      ((composeg
        (== fs x)
        (remcg this)) a))
    IConstraintOp
    (rator [_] `featurec)
    (rands [_] [x])
    IReifiableConstraint
    (reifyc [_ v r a]
      (let [fs (into {} fs)
            r  (-reify* r (walk* a fs))]
        `(featurec ~(walk* r x) ~(walk* r fs))))
    IRunnable
    (runnable? [_ a]
      (not (lvar? (walk a x))))
    IConstraintWatchedStores
    (watched-stores [this] #{::subst})))

(defn featurec
  "Ensure that a map contains at least the key-value pairs
  in the map fs. fs must be partially instantiated - that is, 
  it may contain values which are logic variables to support 
  feature extraction."
  [x fs]
  (cgoal (-featurec x (partial-map fs))))

;; =============================================================================
;; defnc

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
                        (tree-term? x) (-ground-term? x s)
                        :else x)))))))]
    (try
      (-ground-term? x s)
      true
      (catch Exception e
        false))))

;; consider ^:partial type hint for arguments
;; these argument only need to be partially instantiated

(defmacro fnc
  "Define an anonymous constraint that can be used with the unifier:

     (let [oddc (fnc [x] (odd? x))]

       (unifier {:a '?a} {:a 1} :when {'?a oddc})
         ;;=> {:a 1}

       (unifier {:a '?a} {:a 2} :when {'?a oddc})
         ;;=> nil
     )

  Use defnc to define a constraint and assign a toplevel var."
  [args & body]
  (let [name (gensym "constraint")
        -name (symbol (str "-" name))]
    `(letfn [(~name [~@args]
               (cgoal (~-name ~@args)))
             (~-name [~@args]
               (reify
                 ~'clojure.lang.IFn
                 (~'invoke [this# a#]
                   (let [[~@args :as args#] (map #(clojure.core.logic/walk* a# %) ~args)
                         test# (do ~@body)]
                     (when test#
                       ((clojure.core.logic/remcg this#) a#))))
                 clojure.core.logic/IConstraintOp
                 (~'rator [_#] '~name)
                 (~'rands [_#] (filter clojure.core.logic/lvar? (flatten ~args)))
                 clojure.core.logic/IReifiableConstraint
                 (~'reifyc [_# _# r# a#]
                   (list '~name (map #(clojure.core.logic/-reify r# %) ~args)))
                 clojure.core.logic/IRunnable
                 (~'runnable? [_# s#]
                   (clojure.core.logic/ground-term? ~args s#))
                 clojure.core.logic/IConstraintWatchedStores
                 (~'watched-stores [_#] #{:clojure.core.logic/subst})))]
       ~name)))

(defmacro defnc [name args & body]
  `(def ~name (fnc ~args ~@body)))

;; =============================================================================
;; Predicate Constraint

(defn -predc
  ([x p] (-predc x p p))
  ([x p pform]
     (reify
       clojure.lang.IFn
       (invoke [this a]
         (let [x (walk a x)]
           (when (p x)
             ((remcg this) a))))
       IConstraintOp
       (rator [_] (if (seq? pform)
                    `(predc ~pform)
                    `predc))
       (rands [_] [x])
       IReifiableConstraint
       (reifyc [c v r a]
         (if (fn? pform)
           (pform c v r a)
           pform))
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
  LCons
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
  [x f reifier]
  (reify
    clojure.lang.IFn
    (invoke [this a]
      (let [x (walk a x)]
        ((composeg (f x a reifier) (remcg this)) a)))
    IConstraintOp
    (rator [_] `fixc)
    (rands [_] [x])
    IReifiableConstraint
    (reifyc [c v r a]
      (if (fn? reifier)
        (reifier c x v r a)
        (let [x (walk* r x)]
          `(fixc ~x ~reifier))))
    IRunnable
    (runnable? [_ a]
      (not (lvar? (walk a x))))
    IConstraintWatchedStores
    (watched-stores [this] #{::subst})))

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

(defn seqc [v]
  (fixc v
    (fn [t _ _]
      (cond
        (sequential? t) succeed
        (lcons? t) (seqc (lnext t))
        :else fail))
    (fn [_ v _ r a]
      `(seqc ~(-reify a v r)))))
