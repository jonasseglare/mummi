(ns mummi.math.affine
  (:require [mummi.math.ops :as ops])
  (:refer-clojure :exclude [* - + == /])
  (:require [mummi.debug :refer [value-of]])
  (:require [mummi.math.utils :refer [mat-to-vec vec-to-col]])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all]))

(defn fit-affine [X Y]
  (transpose
   (ops/least-squares-multi-rhs
    X Y)))

(defn make-homogeneous
  ([x dst-len]
   (if (< (count x) dst-len)
     (conj
      (vec
       (map (fn [i] (if (< i (count x))
                      (get x i) 0))
            (range (- dst-len 1))))
      1) (subvec x 0 dst-len)))
  ([x] (make-homogeneous x (inc (count x)))))


(defn make-homogeneous-rows
  ([X dst-len]
   (map (fn [x] (make-homogeneous x dst-len)) X))
  ([X] (map make-homogeneous X)))


(defn fit-affine-translate [X Y]
  (fit-affine (make-homogeneous-rows X) Y))

(defn apply-affine [T pts]
  (if (every? number? pts)
    (first (apply-affine T [pts]))
    (mmul (make-homogeneous-rows (to-nested-vectors pts) (column-count T))
          (transpose T))))

(defn all-less [a b]
  (assert (= (count a) (count b)))
  (reduce (fn [a b] (and a b)) (map < a b)))

(defn all-equal [inds]
  (let [f (first inds)]
    (every? (fn [x] (= f x)) inds)))

(defn max-elem [s]
  (reduce max s))

(defn compute-square-size
  ([shape]
   (compute-square-size (count shape) (max-elem shape)))
  ([dims me]
   (take dims (repeat me))))

(defn make-mat-or-eye-fun [x]
  (let [src-size (shape x)]
    (fn [& inds]
      (if (all-less inds src-size)
        (apply mget (concat [x] inds))
        (if (all-equal inds) 1 0)))))

(defn make-square
  ([x me]
   (let [dims (count (shape x))
         dst-size (compute-square-size dims me)]
     (compute-matrix
      dst-size
      (make-mat-or-eye-fun x))))
  ([x] (make-square x (max-elem (shape x)))))

(defn mul-affine [a b]
  (let [me (max (max-elem (shape a))
                (max-elem (shape b)))]
    (mmul
     (make-square a me)
     (make-square b me))))
