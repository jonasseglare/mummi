(ns mummi.maprec-test
  (:require [clojure.test :refer :all]
            [mummi.maprec :refer :all]))

(def-map-proto :animal
  get-leg-count
  get-name)

(extend-map-proto
 :animal :Bird
 (get-name
  [x]
  (:bird-name x))
 (get-leg-count
  [x] 2))

(extend-map-proto
 :animal :Cat
 (get-leg-count
  [x] 4)
 (get-name
  [x] (:cat-name x)))

(def-maprec :animal Bird [bird-name])
(def-maprec :animal Cat [cat-name])

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 2 (get-leg-count (make-Bird "thug-life-crow"))))
    (is (= 4 (get-leg-count (make-Cat "Rulle"))))
    (is (= "Rulle" (get-name (make-Cat "Rulle"))))))
