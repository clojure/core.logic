(ns clojure.core.logic.protocols)

;; Marker Interfaces

(definterface IBindable)
(definterface ITreeTerm)
(definterface IVar)

;; =============================================================================
;; Utility Protocols

(defprotocol IUninitialized
  (-uninitialized [coll]))

;; =============================================================================
;; miniKanren Protocols

;; -----------------------------------------------------------------------------
;; Unification protocols for core Clojure types

(defprotocol IUnifyTerms
  (unify-terms [u v s]))

(defprotocol IUnifyWithRecord
  (unify-with-record [u v s]))

(definterface INonStorable)

(defn non-storable? [x]
  (instance? INonStorable x))

;; -----------------------------------------------------------------------------
;; Utility protocols

(defprotocol LConsSeq
  (lfirst [this])
  (lnext [this]))

(defprotocol LConsPrint
  (toShortString [this]))

;; -----------------------------------------------------------------------------
;; Substitution

(defprotocol ISubstitutions
  (ext-no-check [this x v])
  (walk [this x]))

;; -----------------------------------------------------------------------------
;; Protocols for terms

(defprotocol IReifyTerm
  (reify-term [v s]))

(defprotocol IWalkTerm
  (walk-term [v f]))

(defprotocol IOccursCheckTerm
  (occurs-check-term [v x s]))

(defprotocol IBuildTerm
  (build-term [u s]))

;; -----------------------------------------------------------------------------
;; Goal protocols

(defprotocol IBind
  (bind [this g]))

(defprotocol IMPlus
  (mplus [a f]))

(defprotocol ITake
  (take* [a]))

;; -----------------------------------------------------------------------------
;; soft cut & committed choice protocols

(defprotocol IIfA
  (ifa [b gs c]))

(defprotocol IIfU
  (ifu [b gs c]))

;; =============================================================================
;; Rel protocols

(defprotocol IRel
  (setfn [this arity f])
  (indexes-for [this arity])
  (add-indexes [this arity index]))

;; =============================================================================
;; Tabling protocols

(defprotocol ITabled
  (-reify-tabled [this v])
  (reify-tabled [this v])
  (reuse [this argv cache start end])
  (subunify [this arg ans]))

(defprotocol ISuspendedStream
  (ready? [this]))

(defprotocol IAnswerCache
  (-add [this x])
  (-cached? [this x]))

;; =============================================================================
;; cKanren protocols

(defprotocol ISubstitutionsCLP
  (root-val [this x])
  (root-var [this x])
  (ext-run-cs [this x v])
  (queue [this c])
  (update-var [this x v]))

;; -----------------------------------------------------------------------------
;; Constraint Store

(defprotocol IConstraintStore
  (addc [this a c])
  (updatec [this a c])
  (remc [this a c])
  (runc [this c state])
  (constraints-for [this a x ws])
  (migrate [this x root]))

;; -----------------------------------------------------------------------------
;; Generic constraint protocols

;; Step, update the constraint with latest domain information

(defprotocol IConstraintStep
  (-step [c s]))

;; the following assume implementation of -step

(defprotocol IRunnable
  (-runnable? [c]))

(defprotocol IEntailed
  (-entailed? [c]))

(defprotocol IEntailedVar
  (-entailed-var? [c x]))

;; Contraint reflection protocols

(defprotocol IWithConstraintId
  (-with-id [c id]))

(defprotocol IConstraintId
  (-id [c]))

(defn id [c]
  (if (instance? clojure.core.logic.protocols.IConstraintId c)
    (-id c)
    (-> c meta ::id)))

(defn with-id [c id]
  (if (instance? clojure.core.logic.protocols.IWithConstraintId c)
    (-with-id c id)
    (vary-meta c assoc ::id id)))

(defprotocol IConstraintWatchedStores
  (-watched-stores [c]))

(defprotocol IConstraintOp
  (-rator [c])
  (-rands [c]))

(defprotocol IReifiableConstraint
  (-reifyc [c v r a]))

(defn reifiable? [x]
  (instance? clojure.core.logic.protocols.IReifiableConstraint x))

(definterface IEnforceableConstraint)

(defn enforceable? [x]
  (instance? clojure.core.logic.protocols.IEnforceableConstraint x))

;; cgoal

(defprotocol IUnwrapConstraint
  (-unwrap [c]))

;; generic domain related protocols

(defprotocol IMergeDomains
  (-merge-doms [a b]))

(defprotocol IMemberCount
  (-member-count [dom]))

(defprotocol IForceAnswerTerm
  (-force-ans [v x]))

;; -----------------------------------------------------------------------------
;; Tree Constraints

(defprotocol IDisunifyTerms
  (-disunify-terms [u v s cs]))

(definterface ITreeConstraint)

(defn tree-constraint? [x]
  (instance? clojure.core.logic.protocols.ITreeConstraint x))

(defprotocol IPrefix
  (-prefix [c]))

(defprotocol IWithPrefix
  (-with-prefix [c p]))

;; -----------------------------------------------------------------------------
;; Partial Maps

(defprotocol IUnifyWithPMap
  (unify-with-pmap [pmap u s]))

;; -----------------------------------------------------------------------------
;; Deep constraints

(defprotocol IConstrainTree
  (-constrain-tree [t fc s]))

;; -----------------------------------------------------------------------------
;; Features

(defprotocol IFeature
  (-feature [x]))
