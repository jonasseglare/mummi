(ns mummi.signe.controller-test
  (:require [mummi.signe.controller :refer :all])
  (:require [clojure.test :refer :all]))

(deftest controller-test
  (testing "controllers"
    (let [ctrl (make-controller {:a 119})
          a-ctrl (derive-controller ctrl :a)
          b-ctrl (make-controller 120)
          c-ctrl (compose {:a a-ctrl :b b-ctrl})]
      (is (syncable? ctrl))
      (is (= {:a 119} (get-state ctrl)))
      (is (= {:a 119} (deref (get-local-model ctrl))))
      (is (= ctrl (get-root-controller ctrl)))
      (is (= {:a 119} (get-local-state ctrl)))
      (is (= ctrl (get-root-controller a-ctrl)))
      (is (= 119 (get-local-state a-ctrl)))
      (is (= {:a 119 :b 120} (get-state c-ctrl)))
      (is (= {:a 119 :b 120} (get-local-state c-ctrl))))))
