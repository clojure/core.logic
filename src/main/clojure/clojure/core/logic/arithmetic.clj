;   Copyright (c) David Nolen, Rich Hickey, contributors. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.core.logic.arithmetic
  (:refer-clojure :exclude [== = > < >= <=])
  (:use [clojure.core.logic.protocols]
        [clojure.core.logic]))

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
