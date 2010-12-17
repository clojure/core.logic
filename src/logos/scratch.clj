(ns logos.scratch
  (:refer-clojure :exclude [reify ==])
  (:require logos.minikanren)
  (:use [logos.minikanren :only [subst?]])
  (:import [logos.minikanren Substitutions]))

;; more challenges
;; as long as the brunt of the work happens in a lazy sequence
;; we are okay

(defprotocol IBind
  (bind [this g]))

(defprotocol IMPlus
  (mplus [a b]))

(defmacro mplus*
  ([e] e)
  ([e & e-rest]
     `(mplus ~e (mplus* ~@e-rest))))

(defmacro bind*
  ([a g] `(bind ~a ~g))
  ([a g & g-rest]
     `(bind* (bind ~a ~g) ~@g-rest)))

;; MZero
(extend-protocol IBind
  nil
  (bind [_ g]
        nil))

(extend-protocol IMPlus
  nil
  (mplus [_ b]
         b))

;; Unit
(extend-type Substitutions
  IBind
  (bind [this g]
        (g this))
  IMPlus
  (mplus [this b]
         (cond
          (nil? b) this
          (subst? b) (list this b)
          (delay? b) (mplus this (force b))
          :else (lazy-seq (cons this b)))))

;; Inc
(extend-type clojure.lang.Delay
  IBind
  (bind [this g]
        (bind (force this) g))
  IMPlus
  (mplus [this b]
         (mplus (force this) b)))

;; but we only need them in mplus ?
;; scheme uses them when binding two streams as well

;; Stream
(extend-type clojure.lang.LazySeq
  IBind
  (bind [this g]
        (map g this))
  IMPlus
  (mplus [this b]
         (cond
          (nil? b) this
          (subst? b) (lazy-seq (cons b this))
          (delay? b) (mplus this (force b))
          :else (lazy-seq
                 (cons (first this)
                       (cons (first b)
                             (mplus (rest b) (rest this))))))))

(comment
  

  ;; the problem is
  ;; 1) get a mplus goal
  ;; 2) call that mplus goal with a
  ;; 3) this mplus needs to evaluate both of it's arguments
  ;;    if the second argument is a recursive call, BOOM!

  (mplus (bind a (== q 1))
         (delay (bind a (cond-e-fn q))))
  )