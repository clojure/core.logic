(ns clojure.core.logic.arithmetic
  (:refer-clojure :exclude [== = > < >= <=])
  (:use clojure.core.logic))

(defmacro = [x y]
  "Goal for testing whether x and y are equal. Non-relational."
  `(fn [a#]
     (let [wx# (walk a# ~x)
           wy# (walk a# ~y)]
       (if (clojure.core/= wx# wy# )
         a# nil))))

(defmacro > [x y]
  "Goal for testing whether x is greater than y. Non-relational."
  `(fn [a#]
     (let [wx# (walk a# ~x)
           wy# (walk a# ~y)]
       (if (clojure.core/> wx# wy# )
         a# nil))))

(defmacro >= [x y]
  "Goal for testing whether x is greater than or equal to y.
  Non-relational."
  `(fn [a#]
     (let [wx# (walk a# ~x)
           wy# (walk a# ~y)]
       (if (clojure.core/>= wx# wy# )
         a# nil))))

(defmacro < [x y]
  "Goal for testing whether x is less than y. Non-relational."
  `(fn [a#]
     (let [wx# (walk a# ~x)
           wy# (walk a# ~y)]
       (if (clojure.core/< wx# wy# )
         a# nil))))

(defmacro <= [x y]
  "Goal for testing whether x is less than or equal to y.
  Non-relational."
  `(fn [a#]
     (let [wx# (walk a# ~x)
           wy# (walk a# ~y)]
       (if (clojure.core/<= wx# wy#)
         a# nil))))
