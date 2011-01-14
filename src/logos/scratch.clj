(ns logos.scratch
  (:refer-clojure :exclude [reify ==]))

(defprotocol IUnify
  (unify [this other]))

(extend-type Object
  IUnify
  (unify [this other] :unify-default))

(deftype Foo []
  IUnify
  (unify [this other] :unify-foo))

;; Whoa

(dotimes [_ 10]
  (let [f (Foo.)]
   (time
    (dotimes [_ 5e7]
      (unify 1 2)
      (unify f 2)))))

;; BUG: if protocol with other name exists, borked to define on Object

(comment
  (defprotocol IUnify
    (unify [this other]))

  (extend-protocol IUnify
    logos.minikanren.LVar
    clojure.lang.ISeq
    clojure.lang.LConsSeq)

  ;; we still have the problem of dealing with everything
  ;; can't extend to object because we're then forced to
  ;; deal with inheritance issues.

  ;; hmm

  (defn unify- [s u v]
    (cond
     (or (coll? u) (lcons? u) (lvar? u)) (unify u v s)
     (or (identical? u v) (= u v)) s
     :else false))

  (->> '[a0 a1]
       (mapcat (juxt identity #(symbol (str % "'")))))

  ;; we could use unification information to pull apart the other size
  ;; if either side is an identifiable datastructure or an lcons we can
  ;; look see if variables appear in the definition

  ;; first-o
  ;;
  ;; first-o creates a goal
  ;; cons-o greats a goal
  ;;
  ;; first-o should really be
  ;;
  ;; (== a (first l))
  ;;
  ;; rest-o should be
  ;;
  ;; (== a (rest l))

  ;; most of the time is spent in unify and unify seq
  ;; for zebra that makes a lot of sense
  ;; but that time is also primarily spent calling
  ;; lfirst lnext
  )