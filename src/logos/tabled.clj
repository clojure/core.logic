(ns logos.tabled
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren))

;; hmm caches should just be atoms?
(deftype Cache [ansv*])

(defprotocol ISuspendedStream
  (ready? [this]))

(deftype SuspendedStream [^Cache cache ansv* f]
  ISuspendedStream
  (ready? [this]
          (not (= (.ansv* cache) ansv*))))

(defn ss? [x]
  (instance? SuspendedStream x))

;; for disjunction of suspended streams
;; TODO: Sequential is the correct interface to use now I think
;; can probably just extend IBind IMPlus to PersistentVector
(deftype WaitingStream [^clojure.lang.ISeq streams]
  clojure.lang.ISeq
  (first [this] (.first streams))
  (next [this] (let [sn (.next streams)]
                 (and sn (WaitingStream. sn))))
  (more [this] (WaitingStream. (.more streams)))
  (cons [this o] (WaitingStream. (.cons streams o)))
  (count [this] (.count streams))
  (empty [this] (.empty streams))
  (equiv [this o] (.equiv streams o))
  (seq [this] (let [s (.seq streams)]
                (and s (WaitingStream. s)))))

(defn waiting-stream [v]
  (let [v (if (vector? v) v (into [] v))]
   (WaitingStream. v)))

(defn w? [x]
  (instance? WaitingStream x))

(defn w-check [w sk fk]
  (loop [w w a []]
    (cond
     (empty? w) (fk)
     (ready? (first w)) (sk (fn []
                              (let [^SuspendedStream ss (first w)
                                    f (.f ss)
                                    w (waiting-stream (concat a (rest w)))]
                                (if (empty? w)
                                  (f)
                                  (mplus (f) (fn [] w)))))))
    :else (recur (rest w) (conj a (first w)))))

;; TODO: consider the concurrency implications here more closely

(defmacro tabled [args & body]
  `(fn [~@args]
     (let [table# (atom {})
           argv# ~args]
       (fn [a#]
         (let [key# (reify a# argv#)
               cache# (get @table# key#)]
           (if (nil? cache#)
             (let [cache# (atom {})]
               (swap! assoc table# key# cache#)
               ((exist []
                   ~@body
                   (master argv# cache#)) a#))
             (reuse a# argv# cache#)))))))

(defn master [argv cache]
  (fn [a]
    (if (contains? @cache argv)
      nil
      (do
        (swap! conj cache argv)
        a))))

(defprotocol ITabled
  (alpha-equiv? [this x y])
  (reuse [this argv cache])
  (subunify [this arg ans]))

(extend-type logos.minikanren.Substitutions
  ITabled
  (alpha-equiv [this x y]
    (= (reify this x) (reify this y)))
  (reuse [this argv cache])
  (subunify [this arg ans]))

(comment
  (let [x (lvar 'x)
        y (lvar 'y)
        s (to-s [[x 1] [y 2]])]
    (reify s [x y]))
  )