(ns logos.scratch
  (:refer-clojure :exclude [reify ==]))

;; juxt is like cond-e
;; mapcat can combine the result of juxt
;; and we have interleave (it just doesn't keep going)
;; suggest weave

(comment

  (->> '[a0 a1]
       (mapcat (juxt identity #(symbol (str % "'")))))

  ;; we could jus forgo the whole goals thing altogether
  ;; just compile functions that work in the right manner

  ;; can we do bind with some lazy combination of Clojure fns?

  )