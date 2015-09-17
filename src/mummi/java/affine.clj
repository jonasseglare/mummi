(ns mummi.java.affine
  (:import [java.awt.geom AffineTransform])
  (:require [clojure.core.matrix :refer [to-nested-vectors]])
  (:require [mummi.math.affine :refer [fit-affine-translate]]))


(defn make-affine
  ([matrix]
   (let [[[m00 m01 m02]
          [m10 m11 m12]] (to-nested-vectors matrix)]
     (apply
      (fn [a b c d e f] (AffineTransform. a b c d e f))
      (map double [m00 m10 m01 m11 m02 m12]))))
  ([src-pts dst-pts]
   (make-affine
    (fit-affine-translate src-pts dst-pts))))
