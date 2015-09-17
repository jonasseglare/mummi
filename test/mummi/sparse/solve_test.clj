(ns mummi.sparse.solve-test
  (:require
   [clojure.test :refer :all]
   [mummi.sparse.solve :refer :all]))

(deftest solve-sparse-test
  (testing "Sparse least squares"
    (is (is-linear-comb? {:a 1, :b 3}))
    (is (are-linear-combs? [{:a 1, :b 3}]))
    (is (= #{:a :b} (get-linear-comb-keys [{:a 1, :b 3}])))
    (is (= #{:a :b :c :d}
           (get-all-keys [{:a 1, :b 3}] #{:c} {:d 99})))))
