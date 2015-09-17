(ns mummi.db.edges-test
  (:require [clojure.test :refer :all])
  (:require [mummi.db.h2 :as h2])
  (:require [clojure.java.jdbc :as jdbc])
  (:require [mummi.db.utils :as utils])
  (:require [mummi.db.edges :refer :all]))

;; Doesn't work, because for clojure.java.jdbc there
;; is no background server going on.
(defn make-spec [] (h2/make-mem-spec "testdb"))
(def membership "user_member_of_group")

(deftest edges
  (jdbc/with-db-transaction [spec (make-spec)]
    (testing "Add and remove edges to a table"
      (is (make-table spec membership "BIGINT" "BIGINT" false))
      (is (make-table spec membership "BIGINT" "BIGINT" true))
      (is (make-table spec membership "BIGINT" "BIGINT" false))
      (is (make-table spec membership "BIGINT" "BIGINT" true))
      (is (= 0 (total-edge-count spec membership)))
      (is (nil? (get-last-edge spec membership 0 0)))
      (is (add-new-edge spec membership 0 0 nil))
      (is (= 1 (total-edge-count spec membership)))
      (is (add-new-edge spec membership 0 1 nil))
      (is (= 2 (total-edge-count spec membership)))
      (is (nil? (get-last-edge spec membership 119 120))
          (let [edge (get-last-edge spec membership 0 0)]
            (is (= 0 (:src edge)))
            (is (= 0 (:dst edge)))
            (is (:active edge))))
      (is (true? (is-active? spec membership 0 0)))
      (is (false? (is-active? spec membership 0 34)))
      (is (= 2 (total-edge-count spec membership)))
      (is (false? (is-active? spec membership 0 9)))
      (activate spec membership 0 9 nil)
      (is (true? (is-active? spec membership 0 9)))      
      (is (= 3 (total-edge-count spec membership)))
      (activate spec membership 0 9 nil)
      (is (= 3 (count (get-active-edges spec membership))))
      (is (= 3 (total-edge-count spec membership)))
      (deactivate spec membership 0 9 nil)
      (is (false? (is-active? spec membership 0 9)))
      (is (= 4 (total-edge-count spec membership)))
      (deactivate spec membership 0 9 nil)
      (is (= 4 (total-edge-count spec membership)))
      (let [hist (get-edge-history spec membership 0 9)]
        (is (= 2 (count hist)))
        (is (:active (first hist)))
        (is (not (:active (second hist)))))
      (is (= 2 (count (get-active-edges spec membership))))
      (let [edges (get-active-edges spec membership)]
        (is (= (set edges) #{{:src 0 :dst 0} {:src 0 :dst 1}})))
      (set-active-edges-from spec membership 0 [0 1] nil)
      (is (= 4 (total-edge-count spec membership)))
      (activate spec membership 1 31 nil)
      (is (= 5 (total-edge-count spec membership)))
      (is (= 1 (count (get-active-edges-from spec membership 1))))
      (is (= 2 (count (get-active-edges-from spec membership 0))))
      (is (= (get-active-edges-from spec membership 1)
             '({:dst 31 :src 1})))
      (set-active-edges-from spec membership 1 [32] nil)
      (is (= 7 (total-edge-count spec membership)))
      (is (= (get-active-edges-from spec membership 1)
             '({:dst 32 :src 1})))
      (set-active-edges-from spec membership 1 [32] nil)
      (is (= 7 (total-edge-count spec membership)))
      
      )))



;; ;(is (add-new-edge spec table-name src dst time-stamp tr)))    


