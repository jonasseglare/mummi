(ns mummi.plot.swing-test
  (:require [clojure.test :refer :all])
  (:import [javax.imageio ImageIO])
  (:import [java.io File])
  (:require [mummi.plot.swing :refer :all]))


(defn demo0 []
  (let [ctrl (make-vm-ctrl-3d
              [[:points {:color [0 1 0]} [0.3 0.2 0.1]]
               [:linestrip {:color [1 0 0.5]}
                [-100 -100] [-101 -101] [-101 -90]]
               [:line [-3 -3] [1 3]]
               [:text {:color [0 0 0]} "Mjao!!!" [3 3]]])]
    (mummi.gui.common/disp-widget
     (make-default-plot-panel ctrl))))


(defn demo1
  ([fname]
   (disp-plot-2d [:image {:transform [[1 0 0 30] [0 -1 0 900]]} (ImageIO/read (File. fname))]))
  ([] (demo1 "/home/jonas/banner.png")))
