;   Copyright (c) David Nolen, Rich Hickey, contributors. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.core.logic.pldb)

(defn indexed? [v]
  (true? (:index (meta v))))

(defmacro with-dbs [dbs & body]
  `(binding [cljs.core.logic/*logic-dbs* (concat cljs.core.logic/*logic-dbs* ~dbs)]
     ~@body))

(defmacro with-db [db & body]
  `(binding [cljs.core.logic/*logic-dbs* (conj cljs.core.logic/*logic-dbs* ~db)]
     ~@body))

(defmacro db-rel [name & args]
  (let [arity   (count args)
        kname   (str (ns-name *ns*) "/" name "_" arity)
        indexes (vec (map indexed? args))]
    `(def ~name
       (with-meta
         (fn [& query#]
           (fn [subs#]
             (let [dbs# (-> subs# meta :db)
                   facts#
                   (if-let [index# (cljs.core.logic.pldb/index-for-query
                                     subs# query# ~indexes)]
                     (cljs.core.logic.pldb/facts-using-index dbs# ~kname index#
                       (cljs.core.logic/-walk* subs# (nth query# index#)))
                     (cljs.core.logic.pldb/facts-for dbs# ~kname))]
               (cljs.core.logic/to-stream
                 (remove cljs.core.logic/failed?
                   (map (fn [potential#]
                          ((cljs.core.logic/== query# potential#) subs#))
                     facts#))))))
         {:rel-name ~kname :indexes ~indexes}))))
