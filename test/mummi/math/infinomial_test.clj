(ns mummi.math.infinomial-test
  (:require
   [clojure.test :refer :all]
   [mummi.math.infinomial :refer :all]))

(deftest solve-sparse-test
  (testing "Infinomial"
    (is (= 3 (get-weight [3 9])))
    (is (= 9 (get-exponent [3 9])))
    (is (= [3 9] (make-infinomial 3 9)))
    (is (= [-3 9] (infinomial-neg [3 9])))
    (is (= -3 (infinomial-neg 3)))
    (is (= [3 2] (infinomial-abs [-3 2])))
    (is (= [3 2] (infinomial-abs [3 2])))
    (is (infinomial-less? 2 3))
    (is (infinomial-less? -3 -2))
    (is (infinomial-less? -3 2))
    (is (not (infinomial-less? 3 -2)))
    (is (infinomial-less? [2 9] [3 9]))
    (is (infinomial-less? [3 9] [2 10]))
    (is (= [7 2] (infinomial-add [3 2] [4 2])))
    (is (= [3 2] (infinomial-add [3 2] [4 1])))
    (is (= [4 2] (infinomial-add [3 1] [4 2])))
    (is (= 6 (infinomial-mul 2 3)))
    (is (= 4 (infinomial-div 12 3)))
    (is (= [-1 2] (infinomial-sub [3 2] [4 2])))))

    

