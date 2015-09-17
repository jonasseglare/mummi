(ns mummi.serialize
  (:require [mummi.common :as common])
  (:require [mummi.debug :refer [value-of]])
  (:require [clojure.pprint :refer :all])
  (:require [mummi.file :refer [with-out-file]])
  (:import [java.io FileWriter File
            PushbackReader FileReader]))

(defn encode-record [x]
  (cond
    (record? x) {:rec (common/make-record x)}
    (map? x) {:map x}
    :default x))

(defn serialize-records [x]
  (clojure.walk/postwalk
   encode-record
   x))

(defn make-deserializer-map [deserializers]
  (if (map? deserializers)
    deserializers
    (zipmap
     (map :record-name deserializers)
     (map :deserializer deserializers))))

(defn get-record-data [x]
  (if (map? x)
    (get x :rec)))

(defn decode-record [y]
  (if-let [x (get-record-data y)]
    (let [data (common/resolve-record-data (first x))]   
      (require (symbol (:ns data)))
      (let [deserializer
            (ns-resolve (symbol (:ns data))
                        (symbol (:make-from-map-name data)))]
        (assert (not (nil? deserializer))
                (str "Failed to obtain deserializer for " (first x)))
        (let [result
              (deserializer
               (second x))]
          (assert (record? result))
          result)))
    (if (map? y) (:map y) y)))


(defn deserialize-records [x]
  (clojure.walk/prewalk
   decode-record
   x))

; Be careful with:
;  * atoms
;  * channels
;  * functions
; Remove them before serializing. There shouldn't be
; any such objects in the object once everything is stopped.

; Having this 
; (binding [*print-dup* true]

;http://stackoverflow.com/questions/17991565/clojure-defrecord-serialization-classnotfoundexception
(defn serialize-raw
  [data-structure #^String filename]
  (spit filename (pr-str data-structure))
  data-structure)


;; Serialize with custom serializers
(defn serialize [x filename]
  (serialize-raw
   (serialize-records x)
   filename)
  x)

;; This allows us to then read in the structure at a later time, like so:
(defn deserialize-raw [filename]
  (with-open [r (PushbackReader. (FileReader. filename))]
    (read r)))


;; Deserialize with custom deserializers
(defn deserialize [filename]
  (deserialize-records
   (deserialize-raw filename)))


(defn try-deserialize-sub [filename]
  (let [file (File. filename)]
    (if (and (.exists file) (.isFile file))
      (try
        [:success (deserialize filename)]
        (catch Throwable e
          [:exception e]))
      [:missing file])))

(defn try-deserialize [filename]
  (try-deserialize-sub filename))

;; (defn serialize [data-structure filename]
;;   (println
;;    data-structure
;;    (java.io.FileWriter.
;;     (File. filename))))

;; (defn deserialize [filename]
;;   (let [file (File. filename)]
;;     (if (and (.exists file) (.isFile file))
;;       (with-open [r (PushbackReader.
;;                      (FileReader. file))]
;;         (read r)))))
