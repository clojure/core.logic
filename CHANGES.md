From 0.6.5 to 0.6.6
====

Enhancements
---
* metadata support for defne and friends
* remove unqualified symbols from defrel and defrel support macros
* default conde style now wraps conde clauses in vectors
* no need to use ?foo in patterns to declare logic variables
* two new non-relational helpers: pred, is

Fixes
---
* fix broken all macro
* can now define facts on relations from other namespaces
* fix lingering reference to old namespace in all macro
* removed some redundant unification cases
* improved docstrings, fixed typos

From 0.6.4 to 0.6.5
====

Enhancements
---
* Consolidate all the useful name spaces into clojure.core.logic
* We now only overload ==, no more need to exclude reify or inc

You can use core.logic in your own projects with:

```clojure
(ns foo.bar
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))
```
