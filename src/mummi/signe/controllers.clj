(ns mummi.signe.controllers
  (:require [mummi.signe.controller :refer :all])
  (:require [mummi.quantity :as q])
  (:require [mummi.common :refer [try-parse-double try-parse-int]]))

(defn labeled [label ctrl]
  (derive-controller ctrl (fn [x] (str label ": " x))))

(defn int2str [num-controller]
  (assert (Controller? num-controller))
  (derive-controller
   num-controller
   (fn [x]
     (str x))
   (fn [x y]
     (if-let [parsed (try-parse-int y)]
       parsed x))))

(defn time2seconds [time-controller]
  (assert (Controller? time-controller))
  (derive-controller
   time-controller
   (fn [x]
     (str (q/get-seconds x)))
   (fn [x y]
     (if-let [parsed (try-parse-double y)]
       (q/make-seconds parsed)
       x))))
     

(defn float2str [num-controller]
  (assert (Controller? num-controller))
  (derive-controller
   num-controller
   (fn [x]
     (str x))
   (fn [x y]
     (if-let [parsed (try-parse-double y)]
       parsed x))))

(defn constrained [ctrl accept?]
  (assert (Controller? ctrl))
  (derive-controller
   ctrl
   identity
   (fn [x y]
     (if (accept? y) y x))))



(defn- make-value-to-index-map-and-inv [label-value-pairs]
  (let [values (map second label-value-pairs)
        inds (range (count values))]
    [(zipmap values inds)
     (zipmap inds values)]))

(defn value-to-list-controller [label-value-pairs value-controller]
  (assert (vector? label-value-pairs))
  (assert (Controller? value-controller))
  (let [labels (map first label-value-pairs)
        [value-to-index index-to-value] (make-value-to-index-map-and-inv
                                         label-value-pairs)]
    (derive-controller
     value-controller
     (fn [value]
       {:index (if-let [index (get value-to-index value)]
                 index -1)
        :items labels})
     (fn [current-value new-list-model]
       (if-let [new-value (get index-to-value (:index new-list-model))]
         new-value
         current-value)))))

(defn quantity-unit-controller [unit-system quantity-controller]
  (derive-controller
   quantity-controller
   (fn [x]
     (:unit x))
   (fn [quantity new-unit]
     (q/convert-to-unit unit-system quantity new-unit))))
