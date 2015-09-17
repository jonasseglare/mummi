(ns mummi.signe.seesaw
  (:require [mummi.gui.common :refer [disp-widget show-message vstack hstack]])
  (:require [mummi.signe.controller :as ctrl])
  (:require [mummi.common :refer [with-field clamp]])
  (:require [mummi.quantity :as q])
  (:require [mummi.log :refer [log-message ensure-logger get-current-logger]])
  (:import [javax.swing JComboBox])
  (:require [mummi.debug :refer [report-errors value-of]])
  (:require [mummi.signe.controllers :as ctrls])
  (:require [mummi.signe.swing :as swing])
  (:require [seesaw.border :refer [empty-border]])
  (:require [seesaw.core :as seesaw])
  (:import [javax.swing JFrame JList])
  (:require [seesaw.chooser :refer [choose-file]])
  (:require [seesaw.mig :as mig]))

(defn make-field-pair-grow-shrink [a b]
  (doto
      (mig/mig-panel
       :constraints ["insets 0" "[grow][shrink]"]
       :items [[a "grow"]
               [b ""]])
    (.setBorder (empty-border :thickness 0))))


;; Make a widget to edit a filename.
(defn make-filename-edit [ctrl]
  (make-field-pair-grow-shrink
   (ctrl/bind (seesaw/text :columns 20) ctrl)
   (ctrl/bind
    (seesaw/button :text "Choose")
    (fn []
      (choose-file
       :success-fn
       (fn [fc file]
         (ctrl/reset-async ctrl (str file))))))))


(defn open-ok-cancel-dialog [title contents-panel fun]
  (seesaw/show!
   (seesaw/pack!
    (seesaw/dialog
     :title title
     :content contents-panel
     :option-type :ok-cancel
     :success-fn
     (fn [& args]
       (fun))))))


;;;;;;;;;;; QUANTITIES
(defn make-quantity-edit [unit-system label-unit-pairs quantity-controller]
  (let [value-ctrl (ctrls/float2str
                    (ctrl/derive-controller quantity-controller :value))
        unit-ctrl (ctrls/value-to-list-controller
                   label-unit-pairs
                   (ctrls/quantity-unit-controller
                    unit-system quantity-controller))]
    (make-field-pair-grow-shrink
     (ctrl/bind-deferred
      (seesaw/text :columns 20)
      value-ctrl)
     (ctrl/bind
      (JComboBox.)
      unit-ctrl))))

(defn make-time-edit [units quantity-controller]
  (let [labels (map (fn [unit] (get q/time-labels unit)) units)
        pairs (vec (map vector labels units))]
    (make-quantity-edit q/time-system pairs quantity-controller)))
    




;;;;;; STRING LIST EDITOR

(defn make-string-add-button [string-vector-controller]
  (seesaw/button
   :text "Add"
   :listen
   [:action
    (fn [x]
      (let [value (seesaw/input "String to add:" :title "Add")]
        (if value
          (ctrl/update-async
           string-vector-controller
           (fn [v]
             (conj v value))))))]))

(defn vremove [v i]
  (vec
   (concat
    (subvec v 0 i)
    (subvec v (+ 1 i)))))

(defn make-string-remove-button [controller]
  (report-errors
   (swing/bind-enabled
    (seesaw/button
     :text "Remove"
     :listen
     [:action (fn [x]
                (log-message "LETS REMOVE IT")
                (ctrl/update-async
                 controller
                 (fn [c]
                   (log-message "Perform swap")
                   (value-of
                    {:index -1
                     :items (vremove (:items c) (:index c))}))))])
    (ctrl/derive-controller
     controller
     (fn [m]
       (not= -1 (:index m)))))))

(defn ensure-valid-index [x]
  (with-field x index
    (clamp index [-1 (- (count (:items x)) 1)])))

(defn move-item [x ch]
  (ensure-valid-index
   (let [items (:items x)
         index (:index x)
         next-index (+ index ch)
         a (get items index)
         b (get items next-index)]
     (if (and (<= 0 next-index)
              (< next-index (count items)))
       {:index next-index
        :items
        (assoc
         (assoc items index b)
         next-index a)}
       x))))

(defn move-up [x]
  (move-item x -1))

(defn move-down [x]
  (move-item x 1))

(defn make-move-up-button [controller]
  (swing/bind-enabled
   (seesaw/button
    :text "Move up"
    :listen
    [:action
     (fn [x]
       (ctrl/update-async controller move-up))])
   (ctrl/derive-controller
    controller
    (fn [x]
      (< 0 (:index x))))))

(defn make-move-down-button [controller]
  (swing/bind-enabled
   (seesaw/button
    :text "Move down"
    :listen
    [:action
     (fn [x]
       (ctrl/update-async controller move-down))])
   (ctrl/derive-controller
    controller
    (fn [x]
      (if (let [i (:index x)
                n (count (:items x))]
            (if (not= i -1)
              (< i (- n 1))))
        true false)))))

(defn make-string-list-edit [string-vector-controller]
  (let [index-controller (ctrl/make-controller -1)
        controller
        (ctrl/compose
         {:index index-controller
          :items string-vector-controller})]
    (vstack
     [
      ;; [(ctrl/bind
      ;;   (seesaw/label :text "")
      ;;   (ctrl/derive-controller controller str)) false]
      [(seesaw/scrollable
        (ctrl/bind (JList.) controller)) true]
      [(hstack
        [[(make-string-add-button string-vector-controller) false]
         [(make-string-remove-button controller) false]
         [(make-move-up-button controller) false]
         [(make-move-down-button controller) false]]) false]])))
    


(defn demo []
  (doto (JFrame.)
    (.add (make-filename-edit (ctrl/make-controller "rulle.clj")))
    (.pack)
    (.show)))

(defn demo2 []
  (disp-widget (make-filename-edit (ctrl/make-controller "rulle.txt"))))

(defn demo3 []
  (disp-widget (make-time-edit [:seconds :minutes] (ctrl/make-controller (q/make-seconds 30)))))
  ;; (disp-widget (make-quantity-edit q/time-system
  ;;                                  [["seconds" :seconds] ["minutes" :minutes]]
  ;;                                  (ctrl/make-controller (q/make-seconds 30)))))

(defn demo4 []
  (disp-widget (make-string-list-edit (ctrl/make-controller ["Rulle" "Havsan"]))))


