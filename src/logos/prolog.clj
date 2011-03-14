(ns logos.logic
  (:refer-clojure :exclude [reify == inc])
  (:use logos.minikanren))

(defn parent [x y]
  (cond-e
   ((== x 'pam) (== y 'bob))
   ((== x 'tom) (== y 'bob))
   ((== x 'tom) (== y 'liz))
   ((== x 'bob) (== y 'ann))
   ((== x 'bob) (== y 'pat))
   ((== x 'pat) (== y 'jim))))

(defn female [x]
  (cond-e
   ((== x 'pam))
   ((== x 'liz))
   ((== x 'pat))
   ((== x 'ann))))

(defn male [x]
  (cond-e
   ((== x 'tom))
   ((== x 'bob))
   ((== x 'jim))))

(defn grand-parent [x y]
  (exist [z]
    (parent x z)
    (parent z y)))

(defn mother [x y]
  (all
   (parent x y)
   (female y)))

(comment
  (run* [q]
        (parent 'tom q))

  (run* [q]
        (mother 'tom q))

  (run* [q]
        (grand-parent 'tom q))

  (defn-e parent [x y]
    ([tom bob])
    ([tom liz])
    ([bob ann])
    ([bob pat])
    ([pat jim]))

  ;; this could be interesting
  (facts parent
    [tom bob]
    [tom liz]
    ...)

  ;; also, both more succinct than Prolog
  ;; and more Lisp-y
  (rel parent
    [tom bob]
    [tom liz]
    ...)

  (defn parents [x y]
    (cond-e
     ((== x 'foo) (== y 'bar))
     (g x y)))

  ;; something like this?
  (do [old-parents parents]
   (defn parents [x y]
     ((== x 'baz) (== 'woz))
     (old-parent x y)))

  ;; extend-rel could package up new definitions as a block
  ;; but we have the problem of *redefinition*
  
  ;; better to put that info in a data structure
  (def parents
       #{})

  ;; drawback, can't use this to find children
  (defn parent [x y]
    ((project [x] (contains? *parents* x))
     (== (*parents* x) y)))

  ;; this would be sweet
  (extend-rel parent
    ([chris john]))

  ;; we don't want extend-rel to cause stack overflow
  ;; so it needs to use lazy-seq or something like that
  ;; but cond-e *already* uses lazy-seq

  ;; arg list ?
  (extend-rel parent [x y]
    ([chris john]))
  )