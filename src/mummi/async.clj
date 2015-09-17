(ns mummi.async
  (require [clojure.core.async :refer [alts!! close!]]))

     
(defn poll! [c]
  ; https://clojuredocs.org/clojure.core.async/alts!
  (first
   (alts!! [c]
           :default nil)))

(defn close-if [c]
  (if c
    (close! c)))
