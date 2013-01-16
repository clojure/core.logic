(ns clojure.core.logic.protocols)

;; -----------------------------------------------------------------------------
;; force-ans support

;; TODO: this is really Mozart/OZ "distribute"

(defprotocol IForceAnswerTerm
  (-force-ans [v x]))
