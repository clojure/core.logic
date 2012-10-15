(ns clojure.core.logic.datomic
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        [datomic.api :only [db q] :as d]))

(defprotocol IUnifyWithDatum
  (unify-with-datum [u v s]))

(extend-type datomic.db.Datum
  IUnifyTerms
  (unify-terms [u v s]
    (unify-with-datum v u s)))

(defn unify-with-datum* [v u s]
  (loop [i 0 v v s s]
    (if (clojure.core/== i 4)
      (when (empty? v) s)
      (when-let [s (unify s (first v) (nth u i))]
        (recur (inc i) (next v) s)))))

(extend-type clojure.lang.Sequential
  IUnifyWithDatum
  (unify-with-datum [v u s]
    (unify-with-datum* v u s)))

(extend-type datomic.db.Datum
  IUnifyWithSequential
  (unify-with-seq [v u s]
    (unify-with-datum* u v s)))

(defn datomic-rel [conn index q]
  (fn [a]
    (let [components (walk* a (take-while #(not (lvar? (walk a %))) q))]
      (to-stream
        (map (fn [datom]
               (unify a q datom))
          (apply d/datoms (db conn) index components))))))

(comment
  ;; start transactor from datomic directory
  ;; bin/transactor config/samples/free-transactor-template.properties

  (def uri "datomic:free://localhost:4334/hello")

  ;; only the first time
  (d/create-database uri)

  (def conn (d/connect uri))

  (def schema-tx
    [{:db/id #db/id[:db.part/db]
      :db/ident :person/name
      :db/valueType :db.type/string
      :db/cardinality :db.cardinality/one
      :db/doc "A person's name"
      :db.install/_attribute :db.part/db}])

  (d/transact conn schema-tx)

  (d/transact conn
    [{:db/id #db/id[:db.part/user]
      :person/name "Bob"}])

  (run* [q]
    (fresh [e a v t]
      (== e 2)
      (== v :db/retract)
      (datomic-rel conn :eavt [e a v t])
      (== q [e a v t])))

  (last (d/datoms (db conn) :aevt))
 )