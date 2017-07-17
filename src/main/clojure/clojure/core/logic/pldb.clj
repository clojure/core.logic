(ns clojure.core.logic.pldb
  (:refer-clojure :exclude [indexed?])
  (:require [clojure.core.logic :as l]))

;; ----------------------------------------

(def empty-db {})

(defmacro with-dbs [dbs & body]
  `(binding [l/*logic-dbs* (concat l/*logic-dbs* ~dbs)]
          ~@body))

(defmacro with-db [db & body]
  `(binding [l/*logic-dbs* (conj l/*logic-dbs* ~db)]
          ~@body))

(defn facts-for [dbs kname]
  (mapcat #(get-in % [kname ::unindexed]) dbs))

(defn facts-using-index [dbs kname index val]
  (mapcat #(get-in % [kname index val]) dbs))

;; ----------------------------------------
(defn rel-key [rel]
  (if (keyword? rel)
    rel
    (:rel-name (meta rel))))

(defn rel-indexes [rel]
  (:indexes (meta rel)))

(defn indexed? [v]
  (true? (:index (meta v))))


(defn contains-lvar? [x]
  (some l/lvar? (tree-seq coll? seq x)))

(defn ground? [s term]
  (not (contains-lvar? (l/walk* s term))))

(defn index-for-query [s q indexes]
  (let [indexable (map #(ground? s %)  q)
        triples (map vector (range) indexable indexes)]
    (first (for [[i indexable indexed] triples
                 :when (and indexable indexed)]
             i))))

(defmacro db-rel [name & args]
  (let [arity
        (count args)

        kname
        (str (ns-name *ns*) "/" name "_" arity)

        indexes
        (vec (map indexed? args))]
    `(def ~name
       (with-meta
         (fn [& query#]
           (fn [subs#]
             (let [dbs#
                   (-> subs# clojure.core/meta :db)

                   facts#
                   (if-let [index# (index-for-query subs# query# ~indexes)]
                     (facts-using-index dbs#
                                        ~kname
                                        index#
                                        (l/walk* subs# (nth query# index#)))
                     (facts-for dbs# ~kname))]
               (l/to-stream (map (fn [potential#]
                                   ((l/== query# potential#) subs#))
                                 facts#)))))
         {:rel-name ~kname
          :indexes ~indexes}))))

;; ----------------------------------------

(defn db-fact [db rel & args]
  (let [key
        (rel-key rel)

        add-to-set
        (fn [current new]
          (conj (or current #{}) new))

        db-with-fact
        (update-in db [key ::unindexed] #(add-to-set %1 args))

        indexes-to-update ;; ugly - get the vector indexes of indexed attributes
        (map vector (rel-indexes rel) (range) args)

        update-index-fn
        (fn [db [is-indexed index-num val]]
          (if is-indexed
            (update-in db [key index-num val] #(add-to-set %1 args))
            db))]
    (reduce update-index-fn db-with-fact indexes-to-update)))

(defn db-retraction [db rel & args]
  (let [key
        (rel-key rel)

        retract-args
        #(disj %1 args)

        db-without-fact
        (update-in db [key ::unindexed] retract-args)

        indexes-to-update ;; also a bit ugly
        (map vector (rel-indexes rel) (range) args)

        remove-from-index-fn
        (fn [db [is-indexed index-num val]]
          (if is-indexed
            (update-in db [key index-num val] retract-args)
            db))]

    (reduce remove-from-index-fn db-without-fact indexes-to-update)))

;; ----------------------------------------
(defn db-facts [base-db & facts]
  (reduce #(apply db-fact %1 %2) base-db facts))

(defn db [& facts]
  (apply db-facts empty-db facts))

(defn db-retractions [base-db & retractions]
  (reduce #(apply db-retraction %1 %2) base-db retractions))


