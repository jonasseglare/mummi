(ns mummi.gui.easydialog
  (:require [mummi.debug :refer [value-of]])
  (:require [mummi.signe.controller :as ctrl])
  (:require [mummi.signe.swing :as ctrlswing])
  (:require [mummi.gui.common :refer [dialog-with-result right-aligned-panel]])
  (:import [javax.swing JPanel JButton JLabel])
  (:import [java.awt.event ActionListener])
  (:require [seesaw.core :as seesaw])
  (:require [seesaw.mig :refer [mig-panel]]))


(defn make-standard-dialog [frame title]  
  (seesaw/custom-dialog
   :on-close :dispose
   :parent frame
   :title title))

(defmacro easy-dialog [frame-title-returnsym & panel-expressions]
  (let [[frame title returnsym] frame-title-returnsym]
    `(let [dialog# (make-standard-dialog ~frame ~title)]
       (dialog-with-result
        [dialog# ~returnsym]
        (seesaw/show!
         (seesaw/pack!
          (doto dialog#
            (.setContentPane ~(cons 'do panel-expressions)))))))))

(defn make-ok-panel [main-panel controller deliver-result]
  (mig-panel
   :constraints ["" "[grow]" "[grow][shrink]"]
   :items [[main-panel "wrap"]
           [(right-aligned-panel
             [(seesaw/button
               :text "OK"
               :listen [:action
                        (fn [x] (deliver-result
                                 (ctrl/get-state controller)))])
              (seesaw/button
               :text "Cancel"
               :listen [:action (fn [x] (deliver-result nil))])])]]))

(defmacro easy-dialog-ok-old [frame-title-controller & panel-expressions]
  (let [[frame title controller] frame-title-controller]
    `(easy-dialog
      [~frame ~title deliver-result#]
      (make-ok-panel
       ~(cons 'do panel-expressions)
       ~controller
       deliver-result#))))


(defmacro easy-dialog-ok [frame-title-controller & panel-expressions]
  (let [[frame title controller] frame-title-controller]
    `(seesaw/show!
      (seesaw/pack!
       (seesaw/dialog
        :parent ~frame
        :title ~title
        :content (do ~@panel-expressions)
        :option-type :ok-cancel
        :success-fn (fn [x#] (ctrl/get-state ~controller)))))))

(defn demo []
  (easy-dialog
   [nil "Rulle" deliver-results]
   (doto (JPanel.)
     (.add (doto (JButton. "Mummi")
             (.addActionListener
              (proxy [ActionListener] []
                (actionPerformed [e]
                  (deliver-results 119)))))))))

(defn demo2 []
  (let [c (ctrl/make-controller "119")]
    (easy-dialog-ok
     [nil "Rulle" c]
     (mig-panel
      :constraints ["" "[grow][grow]" "[shrink]"]
      :items [[(seesaw/label :text "Current value:") "wrap"]
              [(ctrl/bind (seesaw/text) c) ""]]))))
