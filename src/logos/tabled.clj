(ns logos.tabled
  (:refer-clojure :exclude [reify == inc])
  (:use [logos minikanren match])
  (:import [logos.minikanren Choice]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Data Structures
;; (atom []) is cache, waiting streams are PersistentVectors

(defprotocol ISuspendedStream
  (ready? [this]))

(deftype SuspendedStream [cache ansv* f]
  ISuspendedStream
  (ready? [this]
          (not (= @cache ansv*))))

(defn ^SuspendedStream make-ss [cache ansv* f]
  {:pre [(instance? clojure.lang.Atom cache)
         (vector? ansv*)
         (fn? f)]}
  (SuspendedStream. cache ansv* f))

(defn ss? [x]
  (instance? SuspendedStream x))

(defn to-w [s]
  (into [] s))

(defn w? [x]
  (vector? x))

(defn w-check [w sk fk]
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

(defprotocol ITabled
  (-reify-tabled [this v])
  (reify-tabled [this v])
  (alpha-equiv? [this x y])
  (reuse [this argv cache start end])
  (subunify [this arg ans]))

;; CONSIDER: subunify, reify-term-tabled, extending all the necessary types to them

(extend-type logos.minikanren.Substitutions
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
                (= (reify this x) (reify this y)))

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
                        (mplus a-inf (fn [] this)))))))
  ITake
  (take* [this]
         (w-check this
                  (fn [f] (take* f))
                  (fn [] ()))))

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

(defmacro tabled [args & grest]
  `(let [table# (atom {})]
    (fn [~@args]
      (let [argv# ~args]
        (fn [a#]
          (let [key# (reify a# argv#)
                cache# (get @table# key#)]
            (if (nil? cache#)
              (let [cache# (atom [])]
                (swap! table# assoc key# cache#)
                ((exist []
                        ~@grest
                        (master argv# cache#)) a#))
              (reuse a# argv# cache# nil nil))))))))

(comment
  (defn-e arc-o [x y]
    ([:a :b])
    ([:b :a])
    ([:b :d]))

  (def path-o
    (tabled [x y]
      (cond-e
       ((arc-o x y))
       ((exist [z]
          (arc-o x z)
          (path-o z y))))))

  ;; (:b :a :d)
  (run* [q] (path-o :a q))

  ;; 1.5s
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e5]
       (doall (run* [q] (path-o :a q))))))
  )