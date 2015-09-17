(ns mummi.db.h2)

(defn make-file-spec [db-path db-name]
  (assert (string? db-path))
  (assert (string? db-name))
  {:classname "org.h2.Driver"
   :subprotocol "h2:file"
   :subname (str db-path "/" db-name)})

(defn make-mem-spec [db-name]
  {:classname "org.h2.Driver"
   :subprotocol "h2:mem"
   :subname db-name})
