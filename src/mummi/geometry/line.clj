(ns mummi.geometry.line
  (:require [mummi.common :refer [defrecord2]]))

(defrecord2 Line [pos dir])

(defn make-line [pos dir]
  (assert (= (count pos) (count dir)))
  (Line. pos dir))
