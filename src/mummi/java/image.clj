(ns mummi.java.image
  (:require [mummi.image :refer :all])
  (:import [java.awt.Image]))

(extend-type java.awt.Image
  Image
  (get-width [x] (.getWidth x nil))
  (get-height [x] (.getHeight x nil))
  (get-channels [x] nil)
  (get-pixel [x] nil))
