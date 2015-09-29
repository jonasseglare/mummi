(ns mummi.db.edges
  (:require [clojure.java.jdbc :as jdbc])
  (:require [mummi.debug :as debug])
  (:require [mummi.common :as common])
  (:require [mummi.db.utils :as utils]))

(defn active-name [main-name]
  (str main-name "_active_edges"))

(defn make-table-sql-cmd [spec table-name src-type dst-type reset?]
  (str
   (utils/make-common-table-sql spec table-name reset?)
   " (index IDENTITY PRIMARY KEY, src " src-type", dst "
   dst-type ", time TIMESTAMP, active BOOL); "
   (utils/make-common-table-sql spec (active-name table-name) reset?)
   " (src " src-type ", dst " dst-type ", active BOOL, time TIMESTAMP, PRIMARY KEY(src, dst));"))

(def index-count (keyword "count(index)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PRIMITIVE FUNCTIONS

(defn total-edge-count [spec table-name]
  (index-count
   (first (jdbc/query spec (str "SELECT count(index) FROM " table-name)))))

(defn make-table [spec table-name src-index-type dst-index-type reset?]
  (jdbc/db-do-commands
   spec
   (make-table-sql-cmd spec table-name src-index-type dst-index-type reset?)))

(defn get-last-edge [spec table-name src dst]
  (first (jdbc/query
          spec
          [(str "SELECT * FROM " table-name " WHERE src=? AND dst=? ORDER BY index DESC")
           src dst])))

(defn set-active [spec table-name src dst active time-stamp]
  (jdbc/execute!
   spec
   [(str
     "MERGE INTO " (active-name table-name)
     "(src, dst, active, time) KEY (src, dst) VALUES (?, ?, ?, ?)")
    src dst active time-stamp]))

;; Check if an edge is active
(defn is-active? [spec table-name src-index dst-index]
  (if-let [value (jdbc/query
              spec
              [(str "SELECT active FROM " (active-name table-name) " WHERE src=? AND dst=?")
               src-index dst-index])]
    (if (:active (first value)) true false)
    false))

(defn add-new-edge
  ([spec table-name src dst time-stamp active]
   (jdbc/execute!
    spec
    [(str "INSERT INTO " table-name " (src, dst, time, active) VALUES (?, ?, ?, ?)")
     src dst (utils/to-valid-time time-stamp)
     active])
   (set-active spec table-name src dst active time-stamp))
  ([spec table-name src dst time-stamp]
   (add-new-edge spec table-name src dst time-stamp true)))

(defn get-edge-history [spec table-name src dst]
  (jdbc/query
   spec
   [(str "SELECT * FROM " table-name " WHERE src=? AND dst=? ORDER BY index ASC") src dst]))

(defn get-active-edges [spec table-name]
  (jdbc/query
   spec
   (str "SELECT src,dst FROM " (active-name table-name) " WHERE active=true")))

(defn get-edges-from-or-to [spec table-name from-or-to x all?]
  (common/validate-data (fn [x] (contains? #{:from :to} from-or-to)) from-or-to)
  (jdbc/query
   spec
   [(str "SELECT src,dst FROM " (active-name table-name) " WHERE "
         (if all? "" "active=true AND ") (if (= :from from-or-to) "src" "dst")
         "=?")
    x]))


(defn get-edges-from [spec table-name src all?]
  (get-edges-from-or-to spec table-name :from src all?))

(defn get-edges-to [spec table-name src all?]
  (get-edges-from-or-to spec table-name :to src all?))

(defn get-active-edges-from [spec table-name src]
  (get-edges-from spec table-name src false))

(defn get-all-edges-from [spec table-name src]
  (get-edges-from spec table-name src true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUNCTIONS WITH TRANSACTIONS

(defn touch-edge [spec table-name src dst time]
  (jdbc/execute!
   spec
   [(str "UPDATE " (active-name table-name) " SET time=? WHERE src=? AND dst=?")
    time src dst]))

;; Activate a single edge.
(defn activate [spec table-name src-index dst-index time-stamp0]
  (let [time-stamp (utils/to-valid-time time-stamp0)]
    (jdbc/with-db-transaction [conn spec]
      (if (not (is-active? spec table-name src-index dst-index))
        (add-new-edge
         conn table-name src-index dst-index (utils/to-valid-time time-stamp))
        (touch-edge spec table-name src-index dst-index time-stamp)))))


(defn deactivate [spec table-name src-index dst-index time-stamp0]
  (let [time-stamp (utils/to-valid-time time-stamp0)]
    (jdbc/with-db-transaction [conn spec]
      (if (is-active? conn table-name src-index dst-index)
        (add-new-edge
         conn table-name src-index dst-index
         (utils/to-valid-time time-stamp) false)
        (touch-edge spec table-name src-index dst-index time-stamp)))))

(defn set-active-edges-from [spec table-name src dsts0 time-stamp]
  (jdbc/with-db-transaction [conn spec]
    (let [dsts (set dsts0)
          currently-active (set (map :dst (get-active-edges-from spec table-name src)))]
      (doseq [dst (clojure.set/difference currently-active dsts)]
        (deactivate spec table-name src dst time-stamp))
      (doseq [dst (clojure.set/difference dsts currently-active)]
        (activate spec table-name src dst time-stamp)))))
