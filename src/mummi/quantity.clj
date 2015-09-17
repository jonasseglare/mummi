(ns mummi.quantity
  (:require [mummi.common :as common]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,
;;;;;;;;;;;;;;; UnitSystem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,
;
;
; (make-unit-system [[:meters 1000 :millimeters] [:millimeters]])
;
; Extract from u = [:meters :100 :centimeters]
; [:100 :centimeters]
(defn get-unit-def [u]
  (let [temp (subvec u 1)]
    (if (empty? temp) nil
        temp)))
; (get-unit-def [:meters :100 :centimeters])

; Extract from u = [:meters :100 :centimeters]
; :meters
(defn get-unit-key [u]
  (first u))
; (get-unit-key [:meters :100 :centimeters])

; Make a map that defines every unit in terms of another unit
(defn make-unit-map [units]
  (let [keys (map get-unit-key units)
        values (map get-unit-def units)]
    (zipmap keys values)))

; Since the unit definitions form a tree,
; with a root unit, we can query the depth in a
; rooted tree
(defn calc-unit-depth [unit unit-map]
  (let [x (unit-map unit)]
    (if (empty? x) 0
        (+ 1 (calc-unit-depth (second x)
                              unit-map)))))

; Return a vec with [depth-of-unit-key updated-depth-map]
(defn follow-depth-branch [unit-key unit-map depth-map]
  (let [unit-def (unit-map unit-key)]
    (if (empty? unit-def) [0 (assoc depth-map unit-key 0)]
        (let [next-unit (second unit-def)
              [depth updated-depth-map] (follow-depth-branch next-unit
                                                             unit-map
                                                             depth-map)
              this-depth (+ 1 depth)]
          [this-depth (assoc updated-depth-map unit-key this-depth)]))))

; Makes a map with the depth of every unit
(defn make-depth-map [unit-map-or-units]
  (let [unit-map (if (map? unit-map-or-units)
                      unit-map-or-units
                      (make-unit-map unit-map-or-units))]
    (loop [keys (keys unit-map)
           result {}]
      (if (empty? keys) result
          (recur (rest keys)
                 (second (follow-depth-branch (first keys)
                                              unit-map
                                              result)))))))

(defn next-unit [unit unit-map]
  (second (unit-map unit)))

(defn get-common-unit [unit-a unit-b unit-map unit-depth-map]
  (if (= unit-a unit-b)
    unit-a
    (let [a-depth (unit-depth-map unit-a)
          b-depth (unit-depth-map unit-b)
          dif (- (unit-depth-map unit-a)
                 (unit-depth-map unit-b))
          next-a (if (<= 0 dif)
                   (next-unit unit-a unit-map)
                   unit-a)
          next-b (if (>= 0 dif)
                   (next-unit unit-b unit-map)
                   unit-b)]
      (get-common-unit
       next-a
       next-b
       unit-map
       unit-depth-map))))

(defn calc-unit-factor [query-unit parent-unit unit-map]
  (if (= query-unit parent-unit) 1.0
      (let [unit-def (unit-map query-unit)]
        (* (first unit-def)
           (calc-unit-factor (second unit-def)
                             parent-unit
                             unit-map)))))

(defn make-unit-conversion-table [unit-map]
  (let [depth-map (make-depth-map unit-map)
        units (keys unit-map)
        n (count unit-map)
        n2 (* n n)
        key-index-pairs (map
                         (fn [index]
                           (common/get-pair-inds
                            index
                            n))
                         (range n2))
        key-pairs (map
                   (fn [index-pair]
                     [(nth units (first index-pair))
                      (nth units (second index-pair))])
                   key-index-pairs)
        values (map
                (fn [pair]
                  (let [common (get-common-unit (first pair)
                                                (second pair)
                                                unit-map
                                                depth-map)]
                    (/
                     (calc-unit-factor (first pair) common unit-map)
                     (calc-unit-factor (second pair) common unit-map))))
                key-pairs)]
    (zipmap key-pairs values)))
                    
(defrecord UnitSystem [unit-map conversion-table])

(defn UnitSystem? [x]
  (instance? UnitSystem x))

(defn make-unit-system [units]
  (let [unit-map (make-unit-map units)]
    (UnitSystem. unit-map (make-unit-conversion-table unit-map))))

(defn get-conversion-factor [unit-system
                             from-unit
                             to-unit]
  (assert (UnitSystem? unit-system))
  (get (:conversion-table unit-system)
       [from-unit to-unit]))

(defn is-unit? [unit-system unit]
  (contains? (:unit-map unit-system) unit))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Quantity
;
(common/defrecord2 Quantity [value unit])

(defn Quantity? [x]
  (instance? Quantity x))

(defn make-quantity [value unit]
  (Quantity. value unit))

(defn get-value [unit-system src-quantity dst-unit]
  (* (get-conversion-factor unit-system (:unit src-quantity) dst-unit)
     (:value src-quantity)))

(defn convert-to-unit [unit-system src-quantity dst-unit]
  (make-quantity
   (get-value unit-system src-quantity dst-unit)
   dst-unit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Macro to define a system
(defmacro def-quantity [quantity-name unit-definitions]
  (let [name-str (name quantity-name)
        units (map first unit-definitions)
        system-name (symbol (str name-str "-system"))
        constructor-name (symbol (str "make-" name-str))
        value-sym (gensym)
        unit-sym (gensym)]
     `(do 
        (def ~system-name (make-unit-system ~unit-definitions))
        (defn ~constructor-name [~value-sym ~unit-sym]
          (make-quantity ~value-sym ~unit-sym))
        ~(cons 'do
               (map (fn [unit]
                      (let [x-sym (gensym)
                            get-function-name (symbol (str "get-" (name unit)))
                            make-function-name (symbol (str "make-" (name unit)))]
                        `(do
                           (defn ~get-function-name [~x-sym]
                             (get-value ~system-name ~x-sym ~unit))
                           (defn ~make-function-name [~x-sym]
                             (~constructor-name ~x-sym ~unit)))))
                    units)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Common physical quantities
(def-quantity time
  [[:weeks 7 :days]
   [:days 24 :hours]
   [:hours 60 :minutes]
   [:minutes 60 :seconds],
   [:seconds 1000 :milliseconds]
   [:milliseconds]])

(def time-labels {:weeks "weeks"
                  :days "days"
                  :hours "hours"
                  :minutes "minutes"
                  :seconds "seconds"
                  :milliseconds "milliseconds"})

(assert (= 24.0 (get-hours (make-days 1))))

(def-quantity length [[:kilometers 1000 :meters]
                      [:meters 10 :decimeters]
                      [:decimeters 10 :centimeters]
                      [:centimeters 10 :millimeters]
                      [:nautical-miles 1852 :meters]])

(def-quantity angle [[:degrees (/ Math/PI 180) :radians]
                     [:radians]])

(def-quantity velocity [[:meters-per-second (/ 3600 1000) :kilometers-per-hour]
                        [:kilometers-per-hour]])

(def-quantity mass [[:kilograms 1000 :grams]
                    [:grams]])


