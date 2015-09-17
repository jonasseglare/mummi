(ns mummi.plot.plotter-test
  (:require [clojure.test :refer :all])
  (:require [mummi.plot.plotter :refer :all]))

(deftest plotter
  (testing "plotter"
    (is (make-plot-view-2d))))
