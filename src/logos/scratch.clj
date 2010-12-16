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
          :else (lazy-seq
                    (cons this
                          b)))))

;; Stream
(extend-type clojure.lang.LazySeq
  IBind
  (bind [this g]
        (map g this))
  IMPlus
  (mplus [this b]
         (cond
          (nil? b) this
          (subst? b) (lazy-seq
                      (cons b this))
          :else (lazy-seq
                 (cons (first this)
                       (cons (first b)
                             (mplus (rest b) (rest this))))))))

(comment
  )