(ns mummi.file
  (:require [clojure.java.io :as io])
  (:import [java.io File])
  (:require [mummi.string :refer [string-to-data]]))

(defn File? [x]
  (instance? File x))

(defn get-lines [file]
  (if (string? file)
    (get-lines (File. file))
    (with-open [rdr (io/reader file)]
      (vec (line-seq rdr)))))

(defn add-line-to-file [file line]
  (with-open [wrtr (io/writer file :append true)]
    (.write wrtr (str line "\n"))))

(defn write-lines [file lines]
  (with-open [wrtr (io/writer file)]
    (doseq [line lines]
      (.write wrtr (str line "\n"))))) 

(defn to-file [x]
  (if (instance? File x) x
      (File. x)))

; Create directory:
; http://stackoverflow.com/questions/3634853/how-to-create-a-directory-in-java
(defn mkdir [x]
  (if (string? x)
    (mkdir (File. x))
    (try
      (.mkdir x)
      true
      (catch Exception e
        false))))

(defn load-numbers [filename]
  (map string-to-data (get-lines filename)))

; Binds a file to all printing.
(defmacro with-out-file [opts & expr]
  (assert (vector? opts))
  (let [filename (first opts)
        apnd (second opts)
        file (gensym)]
    `(let [~file (io/writer (File. ~filename) :append ~apnd)]
       (clojure.core/binding [clojure.core/*out* ~file]
         ~@expr))))
