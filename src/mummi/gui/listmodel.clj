(ns mummi.gui.listmodel
  (:require [mummi.common :refer [vecseq?]])
  (:import [javax.swing DefaultListModel])
  (:import [javax.swing JList])
  (:require [mummi.gui.common :refer [disp-widget]])
  (:require [mummi.signe.core :as signe]))

(def data (ref [1 2 3]))

(defn make-list-model [items]
  (let [model (javax.swing.DefaultListModel.)]
    (doseq [item items]
      (.addElement model item))
    model))

(defn make-updated-list-model [mon signe-funcall]
  (let [listmodel (DefaultListModel.)]
    (signe/register-and-update
     mon
     signe-funcall
     (fn [model-change]
       (let [items (signe/get-new-value model-change)]
         (assert (vecseq? items))
         (.clear listmodel)
         (doseq [item items]
           (.addElement listmodel item)))))
    listmodel))


;(def listbox (seesaw.core/listbox :model [])) 

(defn attach-list-model [mon listbox signe-funcall]
  (signe/Monitor? mon)
  (instance? JList listbox)
  (signe/Funcall? signe-funcall)
  (signe/register-and-update
   mon
   signe-funcall
   (fn [model-change]
     (.setModel listbox
                (make-list-model
                 (signe/get-new-value
                  model-change)))))
  listbox)

(comment

  (defn list-model-demo []
    (let [model (atom [0 1 2])
          make-numbered-items (fn [data]
                                (map
                                 (fn [x]
                                   (str "Item " x))
                                 data))
          mon (signe/make-monitor model)]
      (disp-widget
       (JList. (make-updated-list-model
                mon
                (signe/call-chain (make-numbered-items :obj)))))
      model))

  (def model (list-model-demo))
  (swap! model (fn [x] (conj
                        x
                        (count x))))
)

