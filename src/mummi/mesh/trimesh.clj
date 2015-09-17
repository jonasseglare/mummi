(ns mummi.mesh.trimesh
  (:require [mummi.common :refer [vecseq? defrecord2 check rotate]])
  (:require [mummi.file :refer [load-numbers]])
  (:require [mummi.debug :refer [value-of]]))

(defn vertex?
  ([x] (vecseq? x))
  ([x n] (vecseq? x n)))

(defn numvec? [x len]
  (if (vecseq? x len)
    (every? number? x)))

(defn facet? [x]
  (numvec? x 3))

(defn edge? [x]
  (numvec? x 2))

(defn facets? [x]
  (if (vecseq? x)
    (every? facet? x)))

(defn edges? [x]
  (if (vecseq? x)
    (every? edge? x)))

(defn vertex-dims? [vertices]
  (count (first vertices)))

(defn vertices? [x]
  (if (vecseq? x)
    (if (vertex? (first x))
      (let [dims (vertex-dims? x)]
        (every? (fn [y] (vertex? y dims)) x)))))

(defrecord2 TriMesh [facets vertices])

(defn parse-facet-or-vertex [s]
  (assert (string? s))
  (map read-string (clojure.string/split s )))

(defn load-from-trig [basename]
  (TriMesh.
   (check facets? (vec (load-numbers (str basename ".tri"))))
   (check vertices? (vec (load-numbers (str basename ".pts"))))))

(defn list-facet-edges [vertex-inds]
  (map (fn [a b] (sort [a b]))
       vertex-inds
       (rotate vertex-inds 1)))

(defn edge-eq? [a b]
  (assert (edge? a))
  (assert (edge? b))
  (or (= a b) (= (reverse a) b)))

(defn get-facets [mesh-or-facets]
  (if (TriMesh? mesh-or-facets)
    (:facets mesh-or-facets)
    mesh-or-facets))

(defn get-vertices [mesh-or-vertices]
  (if (TriMesh? mesh-or-vertices)
    (:vertices mesh-or-vertices)
    mesh-or-vertices))

(defn list-edges [mesh-or-facets]
  (set (reduce concat (map list-facet-edges (get-facets mesh-or-facets)))))

; Make a map from edges to facets that have a common edge.
(defn make-edge-to-facet-map [mesh-or-facets]
  (let [facets (get-facets mesh-or-facets)
        edges (list-edges facets)
        initial-map (zipmap edges (take (count edges) (repeat #{})))]
    (loop [result initial-map
           inds (range (count facets))]
      (if (empty? inds) result
          (let [i (first inds)
                f (nth facets i)
                e (list-facet-edges f)
                old-vals (map (fn [key] (get result key)) e)
                new-vals (map conj old-vals (take (count e) (repeat i)))]
            (recur (merge result (zipmap e new-vals))
                   (rest inds)))))))

(defn make-test-mesh []
  (TriMesh.
   [[0 1 2] [1 3 2] [2 3 4]]
   [[0 0 0] [1 0 0] [0 1 0] [1 1 0] [0 2 0]]))

(defn set-vertices [mesh new-vertices]
  (assoc mesh :vertices new-vertices))

(defn map-vertices [mesh f]
  (set-vertices mesh
                (value-of (vec (map (fn [v] (f (vec v)))
                                    (:vertices mesh))))))

; (def mesh (load-from-trig "/home/jonas/data/datasets/kinect_paper/mesh"))
