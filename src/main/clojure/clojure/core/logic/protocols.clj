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

(defprotocol IRunnable
  (runnable? [c s]))

(defprotocol IWithConstraintId
  (-with-id [this id]))

(defprotocol IConstraintId
  (-id [this]))

(defn id [c]
  (if (instance? clojure.core.logic.protocols.IConstraintId c)
    (-id c)
    (-> c meta ::id)))

(defn with-id [c id]
  (if (instance? clojure.core.logic.protocols.IWithConstraintId c)
    (-with-id c id)
    (vary-meta c assoc ::id id)))

(defprotocol IConstraintWatchedStores
  (watched-stores [this]))

(defprotocol IConstraintOp
  (rator [this])
  (rands [this]))

(defprotocol IRelevant
  (-relevant? [this s]))

(defprotocol IRelevantVar
  (-relevant-var? [this x]))

(defprotocol IReifiableConstraint
  (reifyc [this v r a]))

(defn reifiable? [x]
  (instance? clojure.core.logic.protocols.IReifiableConstraint x))

(definterface IEnforceableConstraint)

(defn enforceable? [x]
  (instance? clojure.core.logic.protocols.IEnforceableConstraint x))

(defprotocol IUnwrapConstraint
  (unwrap [c]))

(defprotocol IMergeDomains
  (-merge-doms [a b]))

(defprotocol IMemberCount
  (member-count [this]))

(defprotocol IForceAnswerTerm
  (-force-ans [v x]))

;; -----------------------------------------------------------------------------
;; Tree Constraints

(defprotocol IDisunifyTerms
  (disunify-terms [u v s cs]))

(definterface ITreeConstraint)

(defn tree-constraint? [x]
  (instance? clojure.core.logic.protocols.ITreeConstraint x))

(defprotocol IPrefix
  (prefix [this]))

(defprotocol IWithPrefix
  (with-prefix [this p]))

;; -----------------------------------------------------------------------------
;; Partial Maps

(defprotocol IUnifyWithPMap
  (unify-with-pmap [pmap u s]))

;; -----------------------------------------------------------------------------
;; Deep constraints

(defprotocol IConstrainTree
  (-constrain-tree [t fc s]))