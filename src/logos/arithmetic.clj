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
