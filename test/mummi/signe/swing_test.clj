(ns mummi.signe.swing-test
  (:import [javax.swing JTextField JFrame JLabel JTextArea])
  (:require [mummi.gui.gridbag :as gridbag])
  (:require [mummi.gui.common :as gui.common])
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

