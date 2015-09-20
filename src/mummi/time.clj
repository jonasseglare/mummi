(ns mummi.time
  (:import [java.util Calendar])
  (:require [mummi.quantity :as quantity]))

(defn now [] (java.util.Calendar/getInstance))

(defn Calendar? [c]
  (instance? java.util.Calendar c))

(defn destructure-calendar [c]
  (assert (Calendar? c))
  [(.get c java.util.Calendar/YEAR)
   (+ 1 (.get c java.util.Calendar/MONTH))
   (.get c java.util.Calendar/DAY_OF_MONTH)
   (.get c java.util.Calendar/HOUR_OF_DAY)
   (.get c java.util.Calendar/MINUTE)
   (.get c java.util.Calendar/SECOND)])

(defn to-iso-8601-string [cal]
  (apply
   format
   (concat
    ["%4d-%02d-%02d %02d:%02d:%02d"]
    (destructure-calendar cal))))


(defn make-calendar
  ([year month day hour minute second]
   (doto (Calendar/getInstance)
     (.set year (- month 1) day
           hour minute second)))
  ([timevec] (let [[year month day hour minute second] timevec]
               (make-calendar year month day
                              hour minute second))))

(defn calendar-to-time [calendar]
  (assert (Calendar? calendar))
  (quantity/make-milliseconds (.getTimeInMillis calendar)))

(defn now-as-quantity []
  (calendar-to-time (now)))

(defn time-to-calendar [time]
  (doto (Calendar/getInstance)
    (.setTimeInMillis
     (quantity/get-milliseconds time))))



  

; (datenum-to-calendar (calendar-to-datenum (now)))


;(defn timevec-to-num [timevec]
;  (calendar-to-num (ass
