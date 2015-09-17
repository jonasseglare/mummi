(ns mummi.math.utils
  (:require [clojure.pprint :refer :all])
  (:refer-clojure :exclude [* - + == /])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all]))

(set-current-implementation :vectorz)

(defn mat-to-vec [X]
  (if (clojure.core.matrix/vec? X) X
      (let [[rows cols] (shape X)]
        (assert (or (= 1 rows) (= 1 cols)))
        (as-vector X))))

(defn vec-to-col [x]
  (if (vec? x)
    (reshape x (conj (shape x) 1))))

(defn numel [x]
  (reduce * (shape x)))

(defn empty-matrix? [x]
  (= 0 (numel x)))
