(ns mummi.image
  (:require [mummi.common :refer [defprotocol2]]))

(defprotocol2 Image
  (get-width [this])
  (get-height [this])
  (get-channels [this])
  (get-pixel [this pos]))


(defn get-size [image]
  [(get-width image) (get-height image)])

(defn get-corners [image]
  (let [[w h] (get-size image)]
    [[0 0] [w 0] [0 h] [w h]]))
