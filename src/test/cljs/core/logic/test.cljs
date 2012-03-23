(ns cljs.core.logic.test
  (:use-macros
   [clj.core.logic.macros
    :only [run run* == conde fresh defne matche]])
  (:require [cljs.core.logic :as l]))