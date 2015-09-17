(ns mummi.maprec
  (:require [mummi.common :refer [make-symbol]]))



(defn make-full-dispatcher [dispatcher]
  (fn [& args]
    (dispatcher (first args))))

(defn- make-dispatched-methods [dsp methods]
  (map
   (fn [method]
     `(defmulti ~method ~dsp))
   methods))

(defn- make-method [dispatcher value method]
  (let [[name args body] method]
    `(defmethod ~name ~value ~args ~body)))



;;;;; Define a protocol for a dispatcher.
(defmacro def-map-proto [dispatcher & methods]
  (let [full-dsp (gensym)]
    `(let [~full-dsp (make-full-dispatcher ~dispatcher)]
       ~@(make-dispatched-methods full-dsp methods))))

;;;;; Extend a protocol for a dispatcher and a value.
(defmacro extend-map-proto [dispatcher value & methods]
  (let [full-dsp (gensym)]
    `(let [~full-dsp (make-full-dispatcher ~dispatcher)]
       ~@(map (fn [method]
                (make-method dispatcher value method))
              methods))))

;;;;; Define a new map type to simulate a record.
(defmacro def-maprec [key name fields]
  (let [arg-names (map (fn [x] (gensym)) (range (count fields)))
        name-kwd (keyword name)]
    `(do (defn ~(make-symbol name "?") [x#]
           (if (map? x#)
             (= (get x# ~key) ~name-kwd)))
         (defn ~(make-symbol "make-" name) [& args#]
           (assoc
            (zipmap
             ~(vec (map keyword fields))
             args#)
            ~key ~name-kwd)))))
            

(comment
  
  (def-map-proto :cat
    talk
    purr)

  (extend-map-proto
   :cat :rulle
   (talk [this a b]
         (println "Mjao!"))
   (purr [this c d e]
         (println "Frrrrrrrr (vulgärt)")))

  (def-maprec :cat Signe [name])

  (extend-map-proto
   :cat :Signe
   (talk [x]
         (println (str "Jag kallas också " (:name x))))
   (purr [x]
         (println "Lite mer fint spinnande, inte så vulgärt")))

  )
