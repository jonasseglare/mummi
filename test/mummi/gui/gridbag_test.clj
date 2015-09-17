(ns mummi.gui.gridbag-test
  (:import [javax.swing JButton JLabel JFrame JTextArea])
  (:require [clojure.test :refer :all])
  (:require [mummi.gui.gridbag :refer :all]))

(deftest gridbag-test
  (testing "Constraints"
    (is (fill-value? :both))
    (is (not (fill-value? :cat)))
    (is (anchor-value? :center))
    (is (not (anchor-value? :rulle)))
    (is (try
          (check-constraints default-constraints)
          false
          (catch Throwable e
            true)))
    (is (try
          (check-constraints
           (merge default-constraints {:gridx 0 :gridy 0 :widget 'my-widget}))
          true
          (catch Throwable e
            false)))
    (is (instance?
         java.awt.GridBagConstraints
         (build-constraints
          (merge default-constraints {:gridx 0 :gridy 0}))))
    (is (= (apply-inset :horizontal [:left :right] {:horizontal 9 :mjao 30})
           {:left 9 :right 9 :mjao 30}))
    (is (= {:left 9 :right 9 :top 9 :bottom 9}
           (build-insets {:any 9})))
    (is (build-insets-obj {}))))

(defn demo0 []
  (let [frame (JFrame. "Demo0")]
    (doto frame
      (.add (make-gridbag-panel
             [{:gridx 0 :gridy 0 :widget (JLabel. "Rulle mjao mjao")}
              {:gridx 0 :gridy 1 :widget (JButton. "OK")}]))
      (.pack)
      (.show))))

(defn make-button-bar []
  (padded-bar [(JButton. "OK") (JButton. "Cancel")]))

(comment
  (make-gridbag-panel
   {:insets {:any default-padding}}
   [{:gridx 0 :gridy 0 :widget (JButton. "OK") :insets {:right 0}}
    {:gridx 1 :gridy 0 :widget (JButton. "Cancel")}]))

(defn make-demo1-panel []
  (make-gridbag-panel
   [{:gridx 0 :gridy 0 :weightx 1 :weighty 1 :fill :both
     :widget (JTextArea. 20 32)}
    {:gridx 0 :gridy 1 :anchor :line-end :widget (make-button-bar)}]))

(defn demo1 []
  (let [frame (JFrame. "Demo1")]
    (doto frame
      (.add (make-demo1-panel))
      (.pack)
      (.show))))
        
