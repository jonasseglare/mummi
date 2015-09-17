(ns mummi.gui.common
  (:require [mummi.debug :refer [value-of]])
  (:import [javax.swing.border TitledBorder])
  (:import [java.awt.event WindowListener])
  (:require [seesaw.mig :refer [mig-panel]])
  (:require [seesaw.core :as seesaw])
  (:import [java.awt Toolkit GridBagConstraints Dialog$ModalityType Dimension FlowLayout GridBagLayout])
  (:import [javax.swing JPanel BorderFactory JDialog JOptionPane JButton
            SwingUtilities UIManager JFrame JLabel]))



(defn native-look-and-feel []
  (UIManager/setLookAndFeel
   (UIManager/getSystemLookAndFeelClassName)))

(defmacro on-edt [& exprs]
  (SwingUtilities/invokeLater
   (fn []
     ~@exprs)))

(defn ask-question
  ([message] (ask-question "(no title)" message))
  ([title message]
   (let [answer (JOptionPane/showConfirmDialog
                 nil
                 message
                 title
                 JOptionPane/YES_NO_OPTION)]
     (= answer JOptionPane/YES_OPTION))))

; When we are done with the dialog, call (.dispose ...)
(defn make-modal-dialog [title]
  (doto (JDialog.)
    (.setModalityType Dialog$ModalityType/APPLICATION_MODAL)
    (.setTitle title)))

(defn with-border [component w]
  (.setBorder
   component
   (BorderFactory/createEmptyBorder w w w w))
  component)

; https://docs.oracle.com/javase/8/docs/api/javax/swing/JOptionPane.html
(def types
  {:error JOptionPane/ERROR_MESSAGE
   :info JOptionPane/INFORMATION_MESSAGE
   :warning JOptionPane/WARNING_MESSAGE
   :question JOptionPane/QUESTION_MESSAGE
   :plain JOptionPane/PLAIN_MESSAGE})

(defn show-message
  ([message]
   (javax.swing.JOptionPane/showMessageDialog nil message))
  ([message title type]
   (javax.swing.JOptionPane/showMessageDialog nil message title (get types type))))



(defn get-screen-size []
  (.getScreenSize (Toolkit/getDefaultToolkit)))

(defn center-frame [f rel-margin]
  (let [s (get-screen-size)
        diag (Math/sqrt (+ (Math/pow (.getWidth s) 2)
                           (Math/pow (.getHeight s) 2)))
        margin-x (* rel-margin (.getWidth s))
        margin-y (* rel-margin (.getHeight s))]
    (.setLocation f margin-x margin-y)
    (.setSize f
              (- (.getWidth s) (* 2 margin-x))
              (- (.getHeight s) (* 2 margin-y)))
    f))

(defn disp-widget [x]
  (doto (JFrame. "Disp widget")
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    (.add x)
    (.setSize (Dimension. 640 480))
    (.show)))

(defn with-titled-border [panel title]
  (doto panel
    (.setBorder
     (doto (BorderFactory/createTitledBorder title)
       (.setTitleJustification TitledBorder/LEFT)))))
    

(defn make-titled-panel [title]
  (with-titled-border (JPanel.) title))

; To be used with vertical lists of widgets, e.g.
;; (doto (JPanel.)
;;   (.setLayout (GridBagLayout.))
;;   (.add my-widget (make-vstretch-constraints 1 false)))
(defn make-vstretch-constraints [row expand]
  (let [c (GridBagConstraints.)]
    (set! (.gridy c) row)
    (set! (.weightx c) 1.0)
    (set! (.weighty c) (if expand 1.0 0.0))
    (set! (.fill c) (if expand
                      GridBagConstraints/BOTH
                      GridBagConstraints/HORIZONTAL))
    c))

(defn make-gb-constraints [vertical? index expand]
  (let [c (GridBagConstraints.)
        expweight (if expand 1.0 0.0)]
    (set! (.fill c) (if expand
                      GridBagConstraints/BOTH
                      (if vertical?
                        GridBagConstraints/HORIZONTAL
                        GridBagConstraints/VERTICAL)))
    (if vertical?
      (do (set! (.gridy c) index)
          (set! (.weightx c) 1.0)
          (set! (.weighty c) expweight))
      (do (set! (.gridx c) index)
          (set! (.weighty c) 1.0)
          (set! (.weightx c) expweight)))
    c))
                            

(defn make-widget-stack [vertical? widget-stretch-pairs]
  (let [n (count widget-stretch-pairs)
        p (doto (JPanel.)
            (.setLayout (GridBagLayout.)))]
    (doseq [i (range n)]
      (let [[widget stretch] (nth widget-stretch-pairs i)]
        (.add p widget (make-gb-constraints vertical? i stretch))))
    p))

(defn vstack [widget-stretch-pairs]
  (make-widget-stack true widget-stretch-pairs))

(defn hstack [widget-stretch-pairs]
  (make-widget-stack false widget-stretch-pairs))


(defn right-aligned-panel [elements]
  (let [p (JPanel.)]
    (.setLayout p (FlowLayout. FlowLayout/RIGHT))
    (doseq [e elements]
      (.add p e))
    p))


;; See myfbot/gui/createactivity for an example. Or see the demos in easydialog.clj
(defmacro dialog-with-result [dialog-and-resultfun & body]
  (let [[dialog result-fun-sym] dialog-and-resultfun]
    `(let [result-promise# (promise)
           ~result-fun-sym (fn [result#]
                             (deliver result-promise# result#)
                             (.dispose ~dialog))]
       (.addWindowListener
        ~dialog
        (proxy [WindowListener] []
          (windowActivated [e#] )
          (windowClosed [e#]
            (~result-fun-sym nil))
          (windowClosing [e#] 
            (~result-fun-sym nil))
          (windowDeactivated [e#] )
          (windowDeiconified [e#] )
          (windowIconified [e#] )
          (windowOpened [e#] )))
       ~@body
       (deref result-promise#))))

(defn make-mig-items-from-pair [pair]
  [[(seesaw/label :text (first pair)) ""]
   [(second pair) "wrap"]])

(defn make-mig-item-list-from-pairs [pairs]
  (reduce
   concat
   (map
    make-mig-items-from-pair
    pairs)))

(defn make-input-table
  ([title text-widget-pairs]
   (with-titled-border
     (make-input-table text-widget-pairs)
     title))
  ([text-widget-pairs]
   (mig-panel
    :constraints ["" "[grow][grow]" ""]
    :items (make-mig-item-list-from-pairs text-widget-pairs))))

(defn table-demo []
  (disp-widget (make-input-table "Input name" [["Name" (seesaw/text :columns 30)]])))

(defn vstack-demo []
  (disp-widget
   (make-widget-stack
    false
    [[(JButton. "Mjao") true] [(JButton. "Rulle") false]])))

(defn make-frame [title panel]
  (doto (JFrame. title)
    (.setContentPane panel)
    (.pack)))

(defn show-frame [title panel]
  (doto (make-frame title panel)
    (.show)))
