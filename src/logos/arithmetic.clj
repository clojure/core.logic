(ns logos.arithmetic
  (:refer-clojure :exclude [reify == inc = > < >= <= + - * / mod])
  (:use logos.minikanren
        [logos.nonrel :only [project]])
  (:import [logos.minikanren Substitutions]))

(defmacro = [x y]
  `(fn [a#]
     (let [wx# (walk a# ~x)
           wy# (walk a# ~y)]
       (if (clojure.core/= wx# wy# )
         a# nil))))

(defmacro > [x y]
  `(fn [a#]
     (let [wx# (walk a# ~x)
           wy# (walk a# ~y)]
       (if (clojure.core/> wx# wy# )
         a# nil))))

(defmacro >= [x y]
  `(fn [a#]
     (let [wx# (walk a# ~x)
           wy# (walk a# ~y)]
       (if (clojure.core/>= wx# wy# )
         a# nil))))

(defmacro < [x y]
  `(fn [a#]
     (let [wx# (walk a# ~x)
           wy# (walk a# ~y)]
       (if (clojure.core/< wx# wy# )
         a# nil))))

(defmacro <= [x y]
  `(fn [a#]
     (let [wx# (walk a# ~x)
           wy# (walk a# ~y)]
       (if (clojure.core/<= wx# wy#)
         a# nil))))

(comment
  (run 1 [q]
       (== q 100)
       (> q 3))

  ;; TODO : remove tediousness w/ macro
  (defn core-sym [sym]
    (symbol "clojure.core" (str sym)))

  ;; macro-writing macro
  (defmacro defarithmetic [op]
    (let [a (gensym "a")]
      `(defmacro ~op [x# y#]
         `(fn [~'a]
            (let [wx# (walk ~'a ~x#)
                  wy# (walk ~'a ~y#)]
              (if (~~(core-sym op) wx# wy#)
                ~'a nil))))))
  )
