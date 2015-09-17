(ns mummi.plot.swing
  (:require [seesaw.core :as seesaw])
  (:require [mummi.gui.common])
  (:require [mummi.plot.graphics2d :as swingplotter])
  (:require [mummi.signe.controller :as controller])
  (:require [mummi.plot.plotter :refer :all])
  (:require [mummi.log :refer :all])
  (:require [mummi.debug :refer [report-errors value-of]])
  (:require [mummi.common :refer [defprotocol2]])
  (:import [java.awt Color])
  (:import [java.awt.Image])
  (:require [mummi.java.image])
  (:import [javax.swing JPanel])
  (:require [mummi.image :refer :all])
  (:import [javax.swing SwingUtilities])
  (:import [java.awt.event MouseMotionListener MouseWheelListener
            WindowAdapter ComponentListener]))

(defn bind-mouse-wheel [panel view-controller]
  (.addMouseWheelListener
   panel
   (proxy [MouseWheelListener] []
     (mouseWheelMoved [e]
       (controller/update-sync
        view-controller
        (fn [vm]
          (try
            (vm-scale-user-view
             vm
             (Math/exp (- (* 0.1 (.getWheelRotation e)))))
            (catch Throwable e
              (log-message "Failed to zoom: " e))))))))
  panel)

(defn disp-to-rot-vec [disp]
  (let [pixel-to-angle 0.01
        [x y] disp]
    [(- (* pixel-to-angle x))
     (* pixel-to-angle y)]))

(defn bind-mouse-motion [panel view-controller]
  (let [translate-gen (swingplotter/make-displacement-gen)]
    (.addMouseMotionListener
     panel
     (proxy [MouseMotionListener] []
       (mouseMoved [e]
         (translate-gen nil))
       (mouseDragged [e]
         (controller/update-sync
          view-controller
          (fn [vm]
            (assert (not (nil? vm)))
            (assert (ViewManager? vm))
            (try
              (let [disp (translate-gen [(.getX e) (.getY e)])]
                (cond
                  (SwingUtilities/isRightMouseButton e)
                  (vm-translate-user-view vm disp)
                  
                  (SwingUtilities/isLeftMouseButton e)
                  (vm-rotate-plot-view vm (disp-to-rot-vec disp))
                  
                  :default vm))
              (catch Throwable e
                (log-message "Caught exception: " e)))))))))
  panel)

(defn bind-mouse [panel vm-ctrl]
  (bind-mouse-motion
   (bind-mouse-wheel panel vm-ctrl)
   vm-ctrl))

(defn bind-size [panel vm-ctrl]
  (let [upd (fn []
              (seesaw/invoke-soon
               (let [new-size [(.getWidth panel) (.getHeight panel)]]
                 (report-errors
                  (controller/update-async
                   vm-ctrl (fn [vm]
                             (set-vm-dst-size
                              vm new-size)))))))]
    (upd)
    (.addComponentListener
     panel (proxy [ComponentListener] []
             (componentHidden [e] )
             (componentMoved [e] )
             (componentResized [e]
               (upd))
             (componentShown [e] )))
    panel))

(defn bind-all [panel vm-ctrl]
  (bind-size (bind-mouse panel vm-ctrl) vm-ctrl))

(defn make-vm-ctrl-2d [plot-ctrl]
  (make-vm-ctrl (make-view-manager-2d) plot-ctrl))

(defn make-vm-ctrl-3d [plot-ctrl]
  (make-vm-ctrl (make-view-manager-3d) plot-ctrl))
  

(defn make-plot-panel [vm-ctrl]
  (let [panel (proxy [JPanel] []
                (paintComponent [g2]
                  (report-errors
                   (let [vm (controller/get-state vm-ctrl)]
                     (proxy-super paintComponent g2)
                     (swingplotter/default-painter-fun
                       g2 vm)))))]
    (controller/subscribe
     vm-ctrl
     (fn [oldv newv]
       (javax.swing.SwingUtilities/invokeLater
        (fn [& args]
          (.setBackground panel (swingplotter/make-color-from-vec
                                 (:bg-color (:opts newv))))
          (.repaint panel)))))
    panel))

(defn make-default-plot-panel [vm-ctrl]
  (bind-all (make-plot-panel vm-ctrl) vm-ctrl))
  
(defn disp-plot-3d [& objs]
  (mummi.gui.common/disp-widget
   (make-default-plot-panel
    (make-vm-ctrl-3d objs))))

(defn disp-plot-2d [& objs]
  (mummi.gui.common/disp-widget
   (make-default-plot-panel
    (make-vm-ctrl-2d objs))))


