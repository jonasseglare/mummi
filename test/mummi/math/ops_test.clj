(ns mummi.math.ops-test
  (:require [clojure.test :refer :all])
  (:require [clojure.core.matrix :as matrix])
  (:require [mummi.math.utils :as utils])
  (:require [mummi.common :as common :refer [near]])
  (:require [mummi.math.ops :refer :all]))

(deftest ops-test
  (testing "Test custom mathematical operations"
    (is (= [[1] [1]]
           (matrix/to-nested-vectors
            (solve-multi-rhs [[2 0] [0 3]] [[2] [3]]))))
    (is (= [[1 2] [1 2]]
           (matrix/to-nested-vectors
            (solve-multi-rhs [[2 0] [0 3]] [[2 4] [3 6]]))))
    (is (near
         (matrix/to-nested-vectors (nullspace [[1 0 0] [0 1 0]]))
         [[0] [0] [1]] 0.0001))
    (is (near
         (matrix/to-nested-vectors
          (solve-with-cst
           (matrix/identity-matrix 3 3)
           [[0] [3] [9]]
           [[1 0 0] [0 1 0]]
           (matrix/zero-matrix 2 1)))
         [[0] [0] [9]]
         0.000001))
))
