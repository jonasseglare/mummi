(ns mummi.signe.swing-test
  (:import [javax.swing JTextField JFrame JLabel JTextArea JList JButton JComboBox
            JSpinner SpinnerNumberModel JCheckBox JRadioButton JPanel])
  (:require [mummi.gui.gridbag :as gridbag])
  (:require [seesaw.core :refer [invoke-later]])
  (:require [mummi.gui.common :as gui.common])
  (:require [mummi.common :as common])
  (:require [mummi.signe.swing :refer :all])
  (:require [mummi.signe.controller :as controller])
  (:require [clojure.test :refer :all]))


(deftest value-filter
  (testing "last value filter"
    (let [f (make-value-filter)]
      (is (= 9 (f 9)))
      (is (nil? (f 9)))
      (is (= 10 (f 10)))
      (is (nil? (f 10))))))


(defn make-instant-text-demo-panel []
  (let [model (controller/make-controller "Edit me...")]
    (gridbag/make-gridbag-panel
     {:insets {:any 5}}
     [{:gridx 0 :gridy 0 :widget (controller/bind (JTextField. 30) model)}
      {:gridx 1 :gridy 0 :widget (controller/bind (JTextField. 30) model)}
      {:gridx 0 :gridy 1 :widget (controller/bind (JLabel. "Rulle") model)}
      {:gridx 1 :gridy 1 :widget (controller/bind-deferred (JTextField. 30) model)}])))

(defn instant-text-demo []
  (gui.common/show-frame
   "Instant text demo"
   (make-instant-text-demo-panel)))

(defn dual-text-area-demo []
  (gui.common/show-frame
   "Text area"
   (let [model (controller/make-controller "")
         chars (controller/derive-controller
                model
                (fn [txt]
                  (str "Number of characters: " (count txt))))]
     (gridbag/make-gridbag-panel
      {:insets {:any 5}}
      [{:gridx 0 :gridy 0 :widget (controller/bind (JTextArea. 20 30) model)}
       {:gridx 1 :gridy 0 :widget (controller/bind-deferred (JTextArea. 20 30) model)}
       {:gridx 0 :gridy 1 :widget (controller/bind (JLabel. "") chars)}]))))


(defn list-demo2 []
  (gui.common/show-frame
   "List demo"
   (let [txt-field (JTextField. 30)
         model (controller/make-controller {:index 0 :items ["Rudolf" "Havsan" "Signe"]})
         name-ctrl (controller/derive-controller
                    model
                    (fn [value]
                      (if (nil? (:index value))
                        ""
                        (if-let [sel (common/nthnil (:items value) (:index value))]
                          (str "Selection: " sel)
                          ""))))]
     (gridbag/make-gridbag-panel
      {:insets {:any 5}}
      [{:gridx 0 :gridy 0 :widget (controller/bind (JList.) model)}
       {:gridx 1 :gridy 0 :widget (controller/bind (JLabel.) name-ctrl)}
       {:gridx 0 :gridy 1 :widget txt-field}
       {:gridx 1 :gridy 1 :widget (controller/bind
                                   (JButton. "Add")
                                   (fn []
                                     (controller/update-sync
                                      model
                                      (fn [value]
                                        (common/with-field value items
                                          (conj items (.getText txt-field)))))))}
       {:gridx 0 :gridy 2 :widget (controller/bind
                                   (JButton. "Move down")
                                   (fn []
                                     (controller/update-sync
                                      model
                                      (fn [value]
                                        (let [items (:items value)
                                              index (:index value)]
                                          (if (nil? index)
                                            value
                                            (if (and (<= 0 index)
                                                     (< (inc index) (count items)))
                                              (let [a (nth items index)
                                                    b (nth items (inc index))]
                                                {:items
                                                 (assoc
                                                  (assoc items index b)
                                                  (inc index) a)
                                                 :index (inc index)})
                                              value)))))))}]))))
                                                  
                                              

(defn list-demo3 []
  (gui.common/show-frame
   "List demo sum"
   (let [model (controller/make-controller {:inds [] :items [1 2 3 4 5 6 7]})
         sum (controller/derive-controller
              model
              (fn [x]
                (str "Sum: " (reduce + (map (fn [i] (nth (:items x) i)) (:inds x))))))]
     (gridbag/make-gridbag-panel
      {:insets {:any 5}}
      [{:gridx 0 :gridy 0 :widget (controller/bind (JList.) model)}
       {:gridx 0 :gridy 1 :widget (controller/bind (JLabel.) sum)}
       {:gridx 0 :gridy 2 :widget (controller/bind
                                   (JButton. "Select odd")
                                   (fn []
                                     (controller/update-sync
                                      model
                                      (fn [x]
                                        (assoc
                                         x
                                         :inds
                                         (vec
                                          (filter
                                           (fn [index]
                                             (odd? (nth (:items x) index)))
                                           (range (count (:items x))))))))))}]))))

(defn combo-box-demo2 []
  (gui.common/show-frame
   "Combo box demo"
   (let [model (controller/make-controller ["Rudolf" "Havsan"])
         combo-model (controller/compose
                      {:items model :index (controller/make-controller -1)})
         full-model-str (controller/derive-controller combo-model str)
         label-model (controller/derive-controller
                      combo-model
                      (fn [value]
                        (if (and (number? (:index value)) (<= 0 (:index value)))
                          (str "You selected "
                               (common/nthnil (:items value) (:index value)))
                          "")))
         new-name (controller/make-controller "")]
     (gridbag/make-gridbag-panel
      {:insets {:any 5}}
      [{:gridx 0 :gridy 0 :widget (controller/bind (JComboBox.) combo-model)}
       {:gridx 0 :gridy 1 :widget (controller/bind (JLabel.) label-model)}
       {:gridx 0 :gridy 2 :widget (controller/bind (JLabel.) full-model-str)}
       {:gridx 0 :gridy 3 :widget (controller/bind (JTextField. 20) new-name)}
       {:gridx 0 :gridy 4 :widget (controller/bind
                                   (JButton. "Add string")
                                   (fn []
                                     (controller/update-sync 
                                      model
                                      (fn [x]
                                        (conj x (controller/get-state new-name))))))}]))))


(defn make-default-spinner []
  (JSpinner.
   (SpinnerNumberModel.
    (Integer. 1) (Integer. 0) nil (Integer. 1))))

(defn jspinner-demo []
  (gui.common/show-frame
   "JSpinner demo"
   (let [model (controller/make-controller 0)]
     (gridbag/make-gridbag-panel
      {:insets {:any 5}}
      [{:gridx 0 :gridy 0 :widget (JLabel. "Spinner 1")}
       {:gridx 1 :gridy 0 :widget (controller/bind (make-default-spinner) model)}
       {:gridx 0 :gridy 1 :widget (JLabel. "Spinner 2")}
       {:gridx 1 :gridy 1 :widget (controller/bind (make-default-spinner) model)}]))))

(defn checkbox-demo2 []
  (gui.common/show-frame
   "Checkbox demo"
   (let [model (controller/make-controller false)
         false-model (controller/derive-controller
                      model
                      (fn [x]
                        (not x))
                      (fn [dst newv]
                        (not newv)))]
     (gridbag/make-gridbag-panel
      {:insets {:any 5}}
      [{:gridx 0 :gridy 0 :widget (controller/bind (JCheckBox. "True") model)}
       {:gridx 0 :gridy 1 :widget (controller/bind (JCheckBox. "False") false-model)}]))))

(defn radio-button-demo2 []
  (let [model (controller/make-controller :rulle)
        b0 (JRadioButton. "Rulle")
        b1 (JRadioButton. "Havsan")
        sel (controller/derive-controller
             model
             (fn [key]
               (str "You selected "
                    (get {:rulle "Rudolf"
                          :havsan "Havsan"} key))))]
    (bind-radio-buttons {:rulle b0, :havsan b1} model)
    (gui.common/show-frame
     "Radio button demo"
     (gridbag/make-gridbag-panel
      {:insets {:any 5}}
      [{:gridx 0 :gridy 0 :widget b0}
       {:gridx 0 :gridy 1 :widget b1}
       {:gridx 0 :gridy 2 :widget (controller/bind (JLabel.) sel)}]))))

(defn toggle-button-demo2 []
  (let [model (controller/make-controller false)]
    (gui.common/show-frame
     "Toggle button"
     (gridbag/make-gridbag-panel
      {:insets {:any 5}}
      [{:gridx 0 :gridy 0 :widget
        (bind-toggle-button
         (JButton.) ["false" "true"] model)}]))))



(defn panel-demo2 []
  (let [ctrl (controller/make-controller {1 "Rulle", 2 "Mjao"})
        panel (JPanel.)]
    (controller/bind-map
     panel
     ctrl
     (fn [map-ctrl key value value-ctrl]
       (controller/bind (JLabel.) value-ctrl))
     first)
    (gui.common/show-frame
     "Panel demo"
     panel)))

(defn widget-text-demo []
  (gui.common/show-frame
   "Widget text demo"
   (let [model (controller/make-controller "Text...")
         button (JButton.)]
     (controller/bind (get-text button) model)
     (gridbag/make-gridbag-panel
      {:insets {:any 5}}
      [{:gridx 0 :gridy 0 :widget (controller/bind (JTextField.) model)}
       {:gridx 0 :gridy 1 :widget button}]))))
