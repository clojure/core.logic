From 0.8-beta2 to 0.8.0-beta3
====

Enhancements
----
* :when support in the simple unifier
* experimental defc macro for converting predicates into constraints
* add IUnitialized protocol to support defrecord
* many improvements to constraint framework & CLP(FD) functionality

Fixes
----
* LOGIC-75: -force-ans implementations for clojure.lang.IPersistentHashMap & LCons
* LOGIC-73: fix partial-map
* LOGIC-69: fix IWalkTerm implementation
* LOGIC-67: fix defne & related macros in ClojureScript implementation
* LOGIC-64: add support for inequalities to eqfd

From 0.8-beta1 to 0.8.0-beta2
====

Enhancements
----
* Datomic support
* eqfd now supports - and /

Fixes
----
* distinctfd goal behaved badly if argument wasn't ground
* LOGIC-62: distincto bug reveals much larger issues around how we look up constraints.
  because vars can be bound in any order and we use vars to map to constraints in the
  store we need a stable root var. constrained vars are now added as ::unbound in the
  substitution map. This information is used to know if a var is a root.
* partial map unification and easy unification now support in ClojureScript
* LOGIC-61: fix partial map support for CLJS
* LOGIC-63: fix simple unification support for CLJS
* removed deprecated IPrintable from core.logic CLJS, implemented
  IPrintWithWriter
* distinctfdc only needs to watch ::subst

From 0.8-alpha2 to 0.8.0-beta1
====

Enhancements
----
* Improve performance of interval constraints
* LOGIC-49: Partial unfication with maps now possible with partial-map
* LOGIC-57: Improve performance of tabled goals
* eqfd sugar for easy declaration of equations

Fixes
----
* LOGIC-46: Unification with struct maps supported

Changes
----
* Unification with domain types no longer supported
* Unification with sets no longer supported: LOGIC-54 through 56

From 0.8-alpha2 to 0.8-alpha3
====

Fixes
----
* LOGIC-50: Rel relation PersistentHashSet becomes LazySeq after issuing a retraction

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
