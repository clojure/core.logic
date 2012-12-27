(ns clojure.core.logic.nominal
  (:refer-clojure :exclude [== hash])
  (:use [clojure.core.logic :exclude [fresh] :as l])
  (:import [java.io Writer]))

(def ^{:dynamic true} *reify-noms* true)

;; =============================================================================
;; Nominal unification with fresh, hash and tie.
;;
;; Some references / inspiration:
;; alphaKanren - http://www.cs.indiana.edu/~webyrd/alphamk/alphamk.pdf
;; Nominal Unification - http://www.cl.cam.ac.uk/~amp12/papers/nomu/nomu-jv.pdf
;; http://code.google.com/p/iucs-relational-research/source/browse/trunk/lib/minikanren/nominal.sls

;; =============================================================================
;; Nominal unification protocols

(defprotocol IApplyPi
  (apply-pi [t pi]))

;; =============================================================================
;; Nom

(declare nom api)

(deftype Nom [lvar]
  clojure.core.logic.IWalkable
  Object
  (toString [_]
    (str "<nom:" (:name lvar) ">"))
  (hashCode [_]
    (.hashCode lvar))
  (equals [this o]
    (and (.. this getClass (isInstance o))
         (= lvar (:lvar o))))
  clojure.lang.IObj
  (withMeta [this new-meta]
    (nom (with-meta lvar new-meta)))
  (meta [this]
    (meta lvar))
  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [_ k not-found]
    (case k
      :lvar lvar
      :name (:name lvar)
      :oname (:oname lvar)
      not-found))
  clojure.core.logic.IReifyTerm
  (reify-term [v s]
    (ext s v (symbol (str (if *reify-noms* "a" (:oname v)) "_" (count s)))))
  IApplyPi
  (apply-pi [t pi]
    (api t pi)))

(defn nom [lvar]
  (Nom. lvar))

(defn nom? [x]
  (instance? clojure.core.logic.nominal.Nom x))

(defmethod print-method Nom [x ^Writer writer]
  (.write writer (str "<nom:" (:name x) ">")))

(defn- nom-bind [sym]
  ((juxt identity
         (fn [s] `(nom (lvar '~s)))) sym))

(defn- nom-binds [syms]
  (mapcat nom-bind syms))

(defmacro fresh
  "Creates fresh noms. Goals occuring within form a logical
  conjunction."
  [[& noms] & goals]
  `(fn [a#]
     (-inc
      (let [~@(nom-binds noms)]
        (bind* a# ~@goals)))))

;; =============================================================================
;; hash: ensure a nom is free in a term

(declare tie? susp? invert-pi compose-pis disagreement-set tie susp)

(defn make-nom-hash [a]
  (fn [x]
    (not (= x a))))

(defn hash [a t]
  (treec
    t
    #(predc % (make-nom-hash a))
    (fn [_ r s ap x]
      (let [x (walk* s x)
            a (walk* s a)]
        ;; Filter constraints unrelated to reified variables.
        (when (and (symbol? a)  (empty? (->> (list x) flatten (filter lvar?))))
          (symbol (str a "#" x)))))    
    (fn [x]
      (cond
        (and (tie? x) (= (:binding-nom x) a))
        (fn [s] s)

        (susp? x)
        (hash (apply-pi a (invert-pi (:pi x))) (:lvar x))

        :else nil))))

;; =============================================================================
;; Suspension

(declare unify-with-susps- unify-two-susps-)

(deftype Suspension [pi lvar]
  clojure.core.logic.ITreeTerm
  Object
  (toString [_]
    (str "<susp:(" (apply str pi) ")" lvar ">"))
  (hashCode [_]
    (.hashCode lvar))
  (equals [this o]
    (and (.. this getClass (isInstance o))
         (and (= pi (:pi o)) (= lvar (:lvar o)))))
  clojure.lang.IObj
  (withMeta [this new-meta]
    (susp pi (with-meta lvar new-meta)))
  (meta [this]
    (meta lvar))
  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [_ k not-found]
    (case k
      :tovar lvar
      :lvar lvar
      :pi pi
      :name (:name lvar)
      not-found))
  clojure.core.logic.IUnifyTerms
  (unify-terms [v u s]
    (unify-with-susps- u v s))
  clojure.core.logic.IReifyTerm
  (reify-term [v s]
    (let [s (-reify* s pi)]
      (let [s (-reify* s lvar)]
        s)))
  clojure.core.logic.IWalkTerm
  (walk-term [v f]
    (let [fv (walk-term (:lvar v) f)
          fpi (walk-term (:pi v) f)]
      (cond
        (empty? fpi) fv
        (nom? (first (first fpi))) (apply-pi fv fpi)
        :else (susp fpi fv))))
  clojure.core.logic.IOccursCheckTerm
  (occurs-check-term [v x s]
    (occurs-check s x (:lvar v)))
  clojure.core.logic.IConstrainTree
  (-constrain-tree [t fc s]
    (fc (:lvar t) s))
  IApplyPi
  (apply-pi [t pi]
    (susp (compose-pis pi (:pi t)) (:lvar t)))
  IDisunifyTerms
  (disunify-terms [u v s cs]
    (disunify-terms (:lvar u) (apply-pi v (:pi u)) s cs)))

(defn unify-with-susps- [v u s]
  (let [v (walk* s v)
        u (walk* s u)]
    (cond
      (and (susp? v) (susp? u))
      (unify-two-susps- v u s)
      (and (susp? v) (lvar? u))
      (unify-two-susps- v (Suspension. nil u) s)
      (and (lvar? v) (susp? u))
      (unify-two-susps- (Suspension. nil v) u s)
      (susp? v)
      (unify s (:lvar v) (apply-pi u (invert-pi (:pi v))))
      (susp? u)
      (unify s (:lvar u) (apply-pi v (invert-pi (:pi u))))
      :else (unify s v u))))

(defn unify-two-susps- [v u s]
  (if (= (:lvar v) (:lvar u))
    (loop [a* (disagreement-set (:pi v) (:pi u))
           s s]
      (if (empty? a*) s
        (recur (rest a*) (bind s (hash (first a*) (:lvar u))))))
    (unify-terms (:lvar u) (apply-pi v (invert-pi (:pi u))) s)))

(defn susp [pi lvar]
  (if (empty? pi) lvar (Suspension. pi lvar)))

(defn susp? [x]
  (instance? clojure.core.logic.nominal.Suspension x))

(defn- fold-right [f init coll]
  (reduce (fn [x y] (f y x)) init (reverse coll)))

(defn api [a pi]
  (fold-right
   (fn [swap a]
     (cond
      (= a (first swap)) (second swap)
      (= a (second swap)) (first swap)
      :else a))
   a pi))

(extend-protocol IApplyPi
  nil
  (apply-pi [t pi] t)

  Object
  (apply-pi [t pi] t)

  clojure.core.logic.LVar
  (apply-pi [t pi]
    (susp pi t))

  clojure.core.logic.LCons
  (apply-pi [t pi]
    (lcons (apply-pi (lfirst t) pi) (apply-pi (lnext t) pi)))

  clojure.lang.IPersistentCollection
  (apply-pi [t pi]
    (cons (apply-pi (first t) pi) (apply-pi (next t) pi))))

(defn compose-pis [pi1 pi2] (concat pi1 pi2))
(defn invert-pi [pi] (reverse pi))

(defn- in? [seq elm]
  (some #(= elm %) seq))

(defn disagreement-set [pi1 pi2]
  (let [i (fn [a a*]
            (if (or (in? a* a)
                    (= (api a pi1) (api a pi2)))
              a*
              (cons a a*)))
        c (fn [pi a*]
            (reduce
             (fn [a* swap]
               (i (first swap) (i (second swap) a*)))
             a* pi))]
    (c pi1 (c pi2 nil))))

(defmethod print-method Suspension [x ^Writer writer]
  (.write writer (str "<susp:(" (apply str (:pi x)) ")"))
  (print-method (:lvar x) writer)
  (.write writer ">"))

;; =============================================================================
;; tie: bind a nom in a term

(declare tie)

(deftype Tie [binding-nom body]
  clojure.core.logic.ITreeTerm
  Object
  (toString [_]
    (str "<tie:" binding-nom "." body ">"))
  (hashCode [_]
    (.hashCode body))
  (equals [this o]
    (and (.. this getClass (isInstance o))
         (and (= binding-nom (:binding-nom o)) (= body (:body o)))))
  clojure.lang.IObj
  (withMeta [this new-meta]
    (tie (with-meta binding-nom new-meta) body))
  (meta [this]
    (meta binding-nom))
  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [_ k not-found]
    (case k
      :binding-nom binding-nom
      :body body
      not-found))
  clojure.core.logic.IUnifyTerms
  (unify-terms [v u s]
    (cond
      (tie? u)
      (if (= (:binding-nom v) (:binding-nom u))
        (unify s (:body v) (:body u))
        (bind* s
          (fn [s] (unify s
                   (apply-pi (:body v) [[(:binding-nom v) (:binding-nom u)]])
                   (:body u)))
          (hash (:binding-nom u) (:body v))))
      (susp? u)
      (unify-with-susps- v u s)
      :else nil))
  clojure.core.logic.IReifyTerm
  (reify-term [v s]
    (let [s (-reify* s binding-nom)]
      (let [s (-reify* s body)]
        s)))
  clojure.core.logic.IWalkTerm
  (walk-term [v f]
    (tie (walk-term (:binding-nom v) f)
         (walk-term (:body v) f)))
  clojure.core.logic.IOccursCheckTerm
  (occurs-check-term [v x s]
    (occurs-check s x (:body v)))
  clojure.core.logic.IConstrainTree
  (-constrain-tree [t fc s]
    (fc (:body t) s))
  IApplyPi
  (apply-pi [t pi]
    (tie (api (:binding-nom t) pi) (apply-pi (:body t) pi))))

(defn tie [binding-nom body]
  (Tie. binding-nom body))

(defn tie? [x]
  (instance? clojure.core.logic.nominal.Tie x))

(defmethod print-method Tie [x ^Writer writer]
  (.write writer (str " [" (:binding-nom x) "] "))
  (print-method (:body x) writer))
