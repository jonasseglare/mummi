(ns mummi.db.utils
  (:require [mummi.time :as time])
  (:require [clojure.java.jdbc :as jdbc]))

(defn to-valid-time [x]
  (cond
    (instance? java.sql.Timestamp x) x
    (nil? x) (java.sql.Timestamp. (.getTimeInMillis (time/now)))))

(defn get-table-data [spec table-name]
  (jdbc/query spec (str "SELECT * FROM " table-name)))

(defn make-common-table-sql [spec table-name reset?]
  (str
   (if reset? (str "DROP TABLE IF EXISTS " table-name "; ") "")
   "CREATE TABLE "
   (if reset? "" "IF NOT EXISTS ") table-name))
