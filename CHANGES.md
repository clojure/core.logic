From 0.8-alpha1 to 0.8-alpha2
====

Changes
----
* everyo was a pseudo goal, rename to everyg

Enhancements
----
* add distincto relation, similar to distinctfd but uses !=

From 0.7.5 to 0.8-alpha1
====

Enhancements
----
* cKanren extensions implemented

From 0.7.4 to 0.7.5
====

Fixes
---
* Compatible with ClojureScript master

From 0.7.3 to 0.7.4
====

Fixes
---
* fix to-stream

From 0.7.2 to 0.7.3
====

Fixes
---
* users of to-stream should not have to remove failures themselves

From 0.7.1 to 0.7.2
====

Fixes
---
* LOGIC-37: The facts and retractions functions do not take the relation's namespace into account when resolving the index.

From 0.7.0 to 0.7.1
====

Fixes
---
Fix pattern matching so unification works on maps and sets

From 0.6.9 to 0.7.0
====

Fixes
---
* Fix project layout so core.logic can work in both Clojure and ClojureScript applications
* fixed LConsSeq -pr-seq implementation


From 0.6.8 to 0.6.9
====

Enhancements
----
* Initial support for ClojureScriopt

From 0.6.7 to 0.6.8
====

Enhancments
----
* Allow retraction of facts

Fixed
----
* LOGIC-32: fix unification with sets
* LOGIC-29: fix defrel bug where we were not checking recursively for logic vars
* LOGIC-24: fix regression, simple expression allowed in pattern matching again

From 0.6.6 to 0.6.7
====

Fixes
---
* repair logic variable introduction in pattern matching macros

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
