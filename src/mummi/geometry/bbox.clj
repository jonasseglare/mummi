(ns mummi.geometry.bbox
  (:require [mummi.common :refer [defrecord2 ind2sub defprotocol2]])
  (:require [mummi.debug :refer [value-of]]))

(defrecord2 BBox [min max])

(defn defined? [x]
  (assert (BBox? x))
  (and (not (nil? (:min x)))
       (not (nil? (:max x)))))

(defn valid? [x]
  (if (BBox? x)
    (= (count (:min x)) (count (:max x)))))

(defn dims? [x]
  (assert (valid? x))
  (count (:min x)))

(defn to-bbox [x]
  (if (BBox? x) x (BBox. x x)))

(defn expand [a b]
  (let [bbox (to-bbox a)]
    (if (defined? bbox)
      (if (BBox? b)
        (-> bbox
            (expand (:min b))
            (expand (:max b)))
        (BBox.
         (map min (:min bbox) b)
         (map max (:max bbox) b)))
      (to-bbox b))))

(defn make-bbox
  ([] (make-bbox []))
  ([vecs]
   (loop [result (to-bbox nil)
          X vecs]
     (if (empty? X)
       result
       (recur (expand result (first X))
              (rest X))))))

(defn get-span [bbox index]
  [(nth (:min bbox) index) (nth (:max bbox) index)])

(defn center [bbox]
  (assert (defined? bbox))
  (map (fn [a b] (* 0.5 (+ a b)))
       (:min bbox)
       (:max bbox)))

(defn size [bbox]
  (assert (defined? bbox))
  (map (fn [a b] (- b a))
       (:min bbox)
       (:max bbox)))

(defn get-corners [bbox]
  (assert (BBox? bbox))
  (let [d (dims? bbox)
        s (take d (repeat 2))]
    (map (fn [i] ; Corner index
           (map (fn [i minx maxx]
                  (if (= 0 i) minx maxx))
                (ind2sub s i)
                (:min bbox)
                (:max bbox)))
         (range (Math/pow 2 d)))))

(defn make-image-bbox [image-size]
  (make-bbox
   [(take (count image-size) (repeat 0))
    image-size]))
         
(defn map-bbox [f bbox]
  (assert (fn? f))
  (assert (BBox? bbox))
  (BBox.
   (f (:min bbox))
   (f (:max bbox))))

(defn calc-minmax-size [bbox]
  (let [s (size bbox)]
    [(reduce min s) (reduce max s)]))

(defn calc-padding [bbox factor]
  (if (defined? bbox)
    (* factor (second (calc-minmax-size bbox)))
    0))

(defn pad-bbox
  ([bbox] (pad-bbox bbox 0.1))
  ([bbox factor] (pad-bbox bbox factor 1.0e-9))
  ([bbox factor min-thresh]
   (let [padding (let [padding (calc-padding bbox factor)]
                   (if (< padding min-thresh)
                     1.0
                     padding))]
     (BBox.
      (map (fn [x] (- x padding))
           (:min bbox))
      (map (fn [x] (+ x padding))
           (:max bbox))))))
  
(defn diag-length [bbox]
  (assert (BBox? bbox))
  (assert (defined? bbox))
  (Math/sqrt (reduce + (map (fn [x] (* x x)) (size bbox)))))
    
(defprotocol2 Bounded
  (get-bbox [x]))
