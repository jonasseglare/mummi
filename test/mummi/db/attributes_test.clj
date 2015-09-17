(ns mummi.db.attributes-test
  (:require [clojure.test :refer :all])
  (:require [clojure.java.jdbc :as jdbc])
  (:require [mummi.db.h2 :as h2])
  (:require [mummi.db.utils :as utils])
  (:require [mummi.db.attributes :refer :all]))

(deftest naming
  (testing "last name"
    (is (= (last-name "rulle")
           "rulle_lastdata"))))

(def membercount "group_member_count")
(defn make-spec [] (h2/make-mem-spec "testdb"))

(deftest operations
  (jdbc/with-db-transaction [spec (make-spec)]
    (testing "Various operations with attribute database"
      (is (make-table spec membercount "VARCHAR(255)" "INTEGER" false))
      (is (nil? (get-value spec membercount "Malmö")))
      (is (= 0 (entity-count spec membercount)))
      (set-value spec membercount "Malmö" 12093 nil)
      (is (= 12093 (:value (get-value spec membercount "Malmö"))))
      (is (= 1 (entity-count spec membercount)))
      (set-value spec membercount "Malmö" 12094 nil)
      (is (= 12094 (:value (get-value spec membercount "Malmö"))))
      (is (= 1 (entity-count spec membercount)))
      (is (= 1 (count (utils/get-table-data spec (last-name membercount)))))
      (is (= 2 (count (utils/get-table-data spec membercount))))
      (let [h (map (fn [x] (dissoc x :time)) (get-history spec membercount "Malmö"))]
        (is (= h '({:value 12093, :entity "Malmö", :index 1}
                   {:value 12094, :entity "Malmö", :index 2}))))
      (is (= 1 (entity-count spec membercount)))
      (set-value spec membercount "Stockholm" 4003 nil)
      (is (= 2 (entity-count spec membercount)))
      (is (= 2 (count (get-history spec membercount "Malmö"))))
      (is (= 1 (count (get-history spec membercount "Stockholm")))))))


