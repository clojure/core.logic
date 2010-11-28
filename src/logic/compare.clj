(ns logic.compare
  (:require [mini-kanren.core :as mk]
            [clojure.core.unify :as unify]))

;; wow, crazy slow 15-40s
;; and crazy amounts of memory
(dotimes [_ 10]
  (time
   (dotimes [_ 1e6]
     (mk/run q
             (mk/& true q)))))

;; much better 13s
;; but still very, very, very slow
(dotimes [_ 10]
 (time
  (dotimes [_ 1e6]
   (unify/unifier '(?x) '(5)))))