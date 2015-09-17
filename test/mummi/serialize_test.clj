(ns mummi.serialize-test
  (:require
   [mummi.common :refer [defrecord2]]
   [clojure.test :refer :all]
   [mummi.serialize :refer :all]))


(defrecord2 Katt [a b])

(defn make-katt [x]
  x)

(deftest common
  (testing "Various functions"

    ;; (is (not
    ;;      (record?
    ;;       (binding [*data-readers* {'mummi.serialize_test.Katt make-katt}]
    ;;         (read-string "#mummi.serialize_test.Katt{:a 3, :b 4}")))))
           
    (is (= [:success (Katt. 3 4)]
           (do
             (serialize (Katt. 3 4) "/tmp/katt.clj")
             (try-deserialize "/tmp/katt.clj"))))))
