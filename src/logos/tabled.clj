(ns logos.tabled
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren))

(defprotocol ISuspendedStream
  (ready? [this]))

(deftype SuspendedStream [cache ansv* f]
  ISuspendedStream
  (ready? [this]
          (not (= @cache ansv*))))

(defn ss? [x]
  (instance? SuspendedStream x))

(defn to-w [s]
  (into [] s))

(defn w? [x]
  (vector? x))

(defn w-check [w sk fk]
  (loop [w w a []]
    (cond
     (empty? w) (fk)
     (ready? (first w)) (sk (fn []
                              (let [^SuspendedStream ss (first w)
                                    f (.f ss)
                                    w (to-w (concat a (rest w)))]
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

(extend-type clojure.lang.IPersistentVector
  IBind
  (bind [this g]
        (map (fn [^SuspendedStream ss]
               (SuspendedStream. (.cache ss) (.ansv* ss)
                                 (fn [] (bind ((.f ss)) g))))
             this))
  IMPlus
  (mplus [this f]
         (let [a-inf (f)]
           (if (w? a-inf)
             (to-w (concat a-inf this))
             (mplus a-inf (fn [] this)))))
  ITake
  (take* [a] ()))

(comment
  (let [x (lvar 'x)
        y (lvar 'y)
        s (to-s [[x 1] [y 2]])]
    (reify s [x y]))
  )