(ns mummi.common-test
  (:require
   [clojure.test :refer :all]
   [mummi.common :refer :all]))

(def-reduced my-add [a b]
  (+ a b))

(deftest common
  (testing "Various functions"
    (is (=
         [1 4 9 16 25 36]
         (combine-repeated
          (fn [a b]
            (conj a (* b b)))
          []
          [1 2 3 4 5 6])))
    (is (= 7 (my-add 3 4)))
    (is (= 12 (my-add 3 4 5)))))
