(ns mummi.plot.graphics2d
  (:import [javax.swing JPanel JFrame])
  (:import [java.awt Color BasicStroke])
  (:import [java.awt.event MouseMotionListener MouseWheelListener WindowAdapter])
  (:require [mummi.log :refer :all])
  (:require [mummi.debug :refer [value-of report-errors]])
  (:require [mummi.common :refer [defrecord2]])
  (:require [mummi.plot.plotter :refer :all])
  (:require [mummi.math.ops :refer [drop-column]])
  (:require [mummi.java.affine :refer [make-affine]])
  (:require [mummi.gui.fonts :refer [make-font]])
  (:require [mummi.math.affine :refer [mul-affine make-square]])
  (:require [mummi.gui.paintable :refer [make-paintable-jpanel]])
  (:require [clojure.core.matrix :refer [inverse]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Low-level plotting interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord2 GPlotter [vm g2d])

(defn make-color-from-vec [x]
  (let [[r g b] (map (fn [y] (int (* 255 y))) x)]
    (Color. r g b)))

(defn apply-options-but-font [graphics-2d options]
  (doto graphics-2d
    (.setColor (make-color-from-vec (:color options)))
    (.setStroke (BasicStroke. (:line-width options)
                              BasicStroke/CAP_ROUND
                              BasicStroke/JOIN_ROUND)))
  graphics-2d)

(defn apply-options [graphics-2d options]
  (apply-options-but-font graphics-2d options)
  (.setFont graphics-2d (make-font (:font options)))
  graphics-2d)
    

(defn apply-plotter-options [g-plotter options]
  (apply-options (:g2d g-plotter)
                 options))

(defn plot-text-to-gplotter [plotter text position options]
  (let [[x y] (vm-project (:vm plotter) position)]
    (apply-options (:g2d plotter) options)
    (.drawString
     (:g2d plotter)
     text (int x) (int y)))
  plotter)

(defn plot-image-to-gplotter [plotter transform image]
  (assert (instance? java.awt.Image image))
  (.drawImage
   (:g2d plotter) image
   (make-affine (drop-column 
                 (let [aff (vm-get-affine (:vm plotter))]
                   (if transform
                     (mul-affine aff (make-square transform))
                     aff)) 2))
   nil))

(extend-type GPlotter
  Plotter
  (plot-line [plotter from to options]
    (let [g (:g2d plotter)
          [x1 y1] (vm-project (:vm plotter) from)
          [x2 y2] (vm-project (:vm plotter) to)]
      (apply-options g options)
      (.drawLine g x1 y1 x2 y2)
      plotter))
  (combine [this other]
    this)
  
  (plot-marker [plotter position options]
    (let [g (:g2d plotter)
          [cx cy] (vm-project (:vm plotter) position)
          r (:marker-size options)
          r2 (* 2 r)
          x (- cx r)
          y (- cy r)]
      (apply-options g options)
      (.fillOval g x y r2 r2)
      plotter))

  (plot-image [plotter transform image]
    (plot-image-to-gplotter plotter transform image))
  
  (plot-text [plotter position text options]
    (plot-text-to-gplotter
     plotter position text options)))

(defn make-displacement-gen []
  (let [state (atom nil)]
    (fn [new-pos]
      (let [last-state (deref state)]
        (reset! state new-pos)
        (if (nil? last-state) [0 0]
            (if (not (nil? new-pos))
              (vec (map - new-pos last-state))))))))

(defn default-painter-fun [g2 vm]
  (assert (ViewManager? vm))
  (let [plotter (GPlotter. vm g2)]
    (draw-axes plotter vm)
    (render-many plotter (get-objects (:plot vm)))))
