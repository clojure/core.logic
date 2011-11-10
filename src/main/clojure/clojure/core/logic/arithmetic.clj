(ns clojure.core.logic.arithmetic
  (:refer-clojure :exclude [reify == inc = > < >= <=])
  (:use clojure.core.logic))

(defmacro =
  "Goal for testing whether x and y are equal. Non-relational."
  [x y]
  `(fn [a#]
     (let [wx# (walk a# ~x)
           wy# (walk a# ~y)]
       (if (clojure.core/= wx# wy# )
         a# nil))))

(defmacro >
  "Goal for testing whether x is greater than y. Non-relational."
  [x y]
  `(fn [a#]
     (let [wx# (walk a# ~x)
           wy# (walk a# ~y)]
       (if (clojure.core/> wx# wy# )
         a# nil))))

(defmacro >=
  "Goal for testing whether x is greater than or equal to y.
  Non-relational."
  [x y]
  `(fn [a#]
     (let [wx# (walk a# ~x)
           wy# (walk a# ~y)]
       (if (clojure.core/>= wx# wy# )
         a# nil))))

(defmacro <
  "Goal for testing whether x is less than y. Non-relational."
  [x y]
  `(fn [a#]
     (let [wx# (walk a# ~x)
           wy# (walk a# ~y)]
       (if (clojure.core/< wx# wy# )
         a# nil))))

(defmacro <=
  "Goal for testing whether x is less than or equal to y.
  Non-relational."
  [x y]
  `(fn [a#]
     (let [wx# (walk a# ~x)
           wy# (walk a# ~y)]
       (if (clojure.core/<= wx# wy#)
         a# nil))))
