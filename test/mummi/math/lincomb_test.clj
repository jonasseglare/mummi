(ns mummi.math.lincomb-test
  (:require [clojure.test :refer :all]
            [mummi.math.lincomb :refer :all]))

(defrecord MyRec [a b])

(deftest test?
  (testing "Testers"
    (is (sum? [+ 1 2 3]))
    (is (not (sum? [])))
    (is (scaled? [3 :a]))
    (is (= {:a 1 :b 1 1 3} (to-normal-form [+ :a :b 3])))
    (is (= {:a -3 :b 4} (to-normal-form [- [4 :b] [3 :a]])))
    (is (= {:a 3} (to-normal-form [3 :a])))
    (is (= {:a 6} (to-normal-form [3 [2 :a]])))
    (is (= {:a -6} (to-normal-form [- [3 [2 :a]]])))
    (is (= {:a 1} (wrap-in-normal-form :a)))
    (is (= {1 30} (wrap-in-normal-form 30)))
    (is (= {(MyRec. :a :b) 1} (to-normal-form (MyRec. :a :b))))
    (is (= {(MyRec. 9 3) 1} (to-normal-form (MyRec. 9 3))))
    (is (= (scaled-to-normal-form [3 :a]) {:a 3}))
    (is (= (add-normal-forms {:a 1 :b 2} {:a 30}) {:a 31 :b 2}))
    (is (product? [* :a 1 3]))
    (is (not (scaled? [3 :a :b])))
    (is (= [3 0 4] (make-vector [:a :b :c] {:a 3 :c 4})))
    (is (= {:a 7 :b 4}
           (replace-symbols
            {:a0 3 :b0 4 :a 4}
            [[:a0 :a] [:b0 :b]])))
    (is (= {:outputs [:a] :inputs [:b]
            :A  [[2.3]] :B [[-1.0]]
            :Ac [[3.9]] :Bc [[4.6]]}
           (build-system [:b]
                         [[+ [2.3 :a] [1.0 :b]]]
                         [[+ [3.9 :a] [-4.6 :b]]])))
    (is (= (build-system [:b 1]
                         [[+ 94 [2.3 :a] [1.0 :b]]]
                         [[+ [3.9 :a] [-4.6 :b]]])
           {:inputs [:b 1], :outputs [:a],
            :A [[2.3]], :B [[-1.0 -94]], :Ac [[3.9]], :Bc [[4.6 0]]}))
    (is (= (make-scaled [* :a 1 3 4])
           [12 :a]))))
