(ns mummi.db.attributes
  (:require [mummi.debug :as debug])
  (:require [mummi.db.utils :as utils])
  (:require [clojure.java.jdbc :as jdbc]))

(defn last-name [table-name]
  (str table-name "_lastdata"))

(defn make-table [spec table-name entity-type value-type reset?]
  (jdbc/execute!
   spec
   [(str
     (utils/make-common-table-sql spec table-name reset?)
     " (index IDENTITY PRIMARY KEY, entity " entity-type
     ", value " value-type ", time TIMESTAMP); "
     (utils/make-common-table-sql spec (last-name table-name) reset?)
     " (entity " entity-type " PRIMARY KEY, value " value-type ", time TIMESTAMP); ")]))

(defn entity-count [spec table-name]
  ((keyword "count(*)")
   (first
    (jdbc/query
     spec
     (str "SELECT count(*) FROM " (last-name table-name))))))

(defn get-value [spec table-name entity]
  (first
   (jdbc/query
    spec
    [(str "SELECT * FROM " (last-name table-name) " WHERE entity=?")
     entity])))

(defn set-value [spec table-name entity value timestamp0]
  (let [last (get-value spec table-name entity)
        timestamp (utils/to-valid-time timestamp0)]
    (if (or (not last) (not= (:value last) value))
      (jdbc/with-db-transaction [conn spec]
        (jdbc/execute!
         conn
         [(str
           "MERGE INTO " (last-name table-name)
           "(entity, value, time) KEY (entity) VALUES (?, ?, ?)")
          entity value timestamp])
        (jdbc/execute!
         conn
         [(str "INSERT INTO " table-name " (entity, value, time) VALUES (?, ?, ?)")
          entity value timestamp])))))

(defn get-history [spec table-name entity]
  (jdbc/query
   spec
   [(str "SELECT * FROM " table-name " WHERE entity=?") entity]))
