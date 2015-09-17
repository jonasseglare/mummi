(ns mummi.mesh.param
  (:require [mummi.common :refer [defrecord2]])
  (:require [mummi.mesh.trimesh :refer :all])
  (:require [mummi.mesh.planar :refer :all])
  (:require [mummi.math.ops :refer :all])
  (:refer-clojure :exclude [* - + == /])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all])
  (:require [clojure.core.matrix.linear :refer :all]))


(defrecord2 LinearMesh [mesh A basis])

; (def mesh (load-from-trig "/home/jonas/data/datasets/kinect_paper/mesh"))

(defn make-linear-mesh [mesh]
  (let [A (make-planar-mat mesh)]
    (LinearMesh.
     mesh A (make-basis A))))
