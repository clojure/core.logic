From 0.8.0-rc1 to 0.8.0-rc2
====

Breaking Changes
----
* CLP(FD) functionality now lives in it's own namespace, +fd -> fd/+, etc.

Enhancements
----
* explicit constraint id management no longer required

Bug Fixes
----
* LOGIC-98: fd terms in nominal terms return mutiple results
* LOGIC-104: nominal dom representation changed to set
* LOGIC-103: `<=fd` non-termination bug

From 0.8.0-beta5 to 0.8.0-rc1
====

Enhancements
----
* Add `seqc` constraint, this is preferred over `listo` as found in TRS

Bug Fixes
----
* LOGIC-100: undiscard diseqality constraints
* LOGIC-101: fix suprising behavior with vars in nom/hash
* LOGIC-96: fix disequality constraints
* LOGIC-95: fix disequality constraints
* LOGIC-53: core.logic converts defrecords
* LOGIC-88: fix disequality reification
* LOGIC-92: improve interaction between nominal & CLP(FD) functionality
* LOGIC-91: fix `predc` entailment issues

From 0.8.0-beta4 to 0.8.0-beta5
====

Enhancements
----
* run* and it's variants are now fully lazy
* alphaKanren implementation - nominal logic programming now possible

Bug Fixes
----
* LOGIC-90: `<=fd` could diverge

From 0.8.0-beta3 to 0.8.0-beta4
====

Breaking Changes
----
* Type specific unification protocols removed
* Simple application expression in pattern matches no longer supported
* PMap is now INonStorable - cannot be unified with a logic var

Enhancments
----
* add `featurec` which replaces the functionality of `partial-map`
* disequality no longer relies on unification
* added support for deep constraints via `fixc`, `treec` demonstrates usage
* fail faster when unifying counted sequential collections
* add experimental support for `:when` in the simple unifier to support constraints

Bug Fixes
----
* LOGIC-59: tabled goals reset between runs
* many bad interactions between unification, FD constraints and disequality resolved
* LOGIC-77: failing CLP(FD) case where logic vars are aliased
* LOGIC-81: constraint store needs substitutions to find root vars
* LOGIC-82: LCons with logic var tail bound to nil would not unify properly with sequences
* LOGIC-85: another logic var aliasing issue

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
