(ns mummi.math.gausselim-test
  (:require [mummi.common :refer [near]])
  (:require [clojure.test :refer :all])
  (:require [mummi.math.generic.gausselim :refer :all]))

(def A2 [[0.5377   -2.2588]
         [1.8339    0.8622]])

(def B2 [[0.3188] [-1.3077]])

(deftest gausselim-test
  (testing "Gaussian elimination"
    (is (= -4 (get-elim-factor 0 [1] [4])))
    (is (= [0 -20]
           (perform-row-op
            [-4 0]
            [1 5] [4 0])))
    (is (= 1 (get-max-row-index
              0
              [[1 2 3]
               [2 3 4]
               [0 9 20]])))
    (is (= [0 1 3] (range-but-index 4 2)))
    (is (= 2 (get-max-row-index
              1
              [[1 2 3]
               [2 3 4]
               [0 9 20]])))
    (is (= [[0 -20]] (perform-elim 1 0 [1 5] [[4 0] [1 5]])))
    (is (near (solve A2 B2)
              [[-0.5816] [-0.2796]]
              0.01))
    (is (near (solve [[3.0e-12 0] [0 2.0e-12]]
                     [[6.0e-12] [8.0e-12]])
              [[2.0] [4.0]]
              0.01))
    (is (near (solve [[3.0e-12 0] [0 2.0e-12]]
                     [6.0e-12 8.0e-12])
              [2.0 4.0]
              0.01))))
         

