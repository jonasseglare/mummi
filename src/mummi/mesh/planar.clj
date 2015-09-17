(ns mummi.mesh.planar
  (:require [mummi.debug :refer [value-of]])
  (:require [mummi.mesh.trimesh :refer :all])
  (:require [mummi.math.ops :refer :all])
  (:require [mummi.math.utils :refer [mat-to-vec]])
  
  (:refer-clojure :exclude [* - + == /])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all]))

(set-current-implementation :vectorz)

;(def mesh (load-from-trig "/home/jonas/data/datasets/kinect_paper/mesh"))

(defn list-planar-vertex-groups [mesh-or-facets]
  (let [facets (get-facets mesh-or-facets)
        neigh-facet-inds (vals (make-edge-to-facet-map mesh-or-facets))]
    (map (fn [inds]
           (reduce
            clojure.set/union
            (map (fn [i] (set (nth facets i)))
                 inds)))
         neigh-facet-inds)))

(defn list-quad-groups [mesh-or-facets]
  (filter (fn [x] (= 4 (count x)))
          (list-planar-vertex-groups mesh-or-facets)))

(defn calc-vertex-weights [vertices]
  (assert (vertices? vertices))
  (normalise
   (mat-to-vec
    (let [dims (vertex-dims? vertices)]
      (if (= 2 dims)
        (nullspace
         (compute-matrix
          [3 4]
          (fn [i j]
            (if (< i 2)
              (nth (nth vertices j) i)
              1))))
        (do (assert (= 3 dims))
            (calc-vertex-weights
             (:reduced-pts (reduce-dimension vertices 2)))))))))

(defn make-planar-eqs [mesh]
  (let [quads (list-quad-groups mesh)]
    (map (fn [inds0]
           (let [inds (vec inds0)]
             [inds (eseq
                    (calc-vertex-weights
                     (vec (map (fn [i] (nth (:vertices mesh) i)) inds))))]))
         quads)))

(defn make-planar-maps [mesh]
  (map (fn [pair]
         (zipmap (first pair)
                 (second pair)))
       (make-planar-eqs mesh)))

(defn make-planar-mat [mesh]
  (let [maps (make-planar-maps mesh)]
    (compute-matrix
     [(count maps) (count (:vertices mesh))]
     (fn [i j]
       (let [m (nth maps i)]
         (if (contains? m j)
           (get m j)
           0))))))
       

; (value-of (calc-vertex-weights [[0 0 0] [0 1 0] [1 0 0] [1 1 0]]))

   

  
