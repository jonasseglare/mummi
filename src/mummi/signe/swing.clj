(ns mummi.signe.swing
  (:require [mummi.atomic :refer [atom? Atomic?]])
  (:require [mummi.common :refer [with-lock filter-map-by-value
                                  filter-map-by-key map-vals defrecord2]])
  (:require [mummi.signe.controller :refer :all])
  (:require [mummi.signe.controllers :refer :all])
  (:require [mummi.gui.common :refer [disp-widget show-message]])
  (:require [mummi.debug :refer [report-errors value-of]])
  (:import [java.awt.event ActionListener FocusListener ItemListener HierarchyListener
            WindowListener])
  (:require [mummi.gui.common :refer [on-edt]])
  (:require [mummi.log :refer [log-message]])
  (:require [seesaw.core :refer [invoke-soon invoke-now invoke-later]])
  (:import [javax.swing JLabel JButton JTextField JList DefaultListModel
            ListSelectionModel JComboBox DefaultComboBoxModel
            JRadioButton ButtonGroup SwingUtilities
            JPanel JFrame JCheckBox JSpinner SpinnerNumberModel
            JTextArea])
  (:import [javax.swing.event DocumentListener ChangeListener ListSelectionListener]))

(defn add-dispose-listener [frame fun]
  (.addWindowListener
   frame
   (proxy [WindowListener] []
     (windowActivated [e] )
     (windowClosed [e]
       (fun))
     (windowClosing [e] )
     (windowDeactivated [e] )
     (windowDeiconified [e] )
     (windowIconified [e] )
     (windowOpened [e] ))))


;; Closes the controller once the ancestor of a widget, if there ever was one,
;; is no longer displayable (that is it has been disposed)
(defn bind-on-dispose [widget fun]
  (let [state (atom nil)]
    (.addHierarchyListener
     widget
     (proxy [HierarchyListener] []
       (hierarchyChanged [e]
         (swap!
          state
          (fn [x]
            (report-errors
             (let [p (SwingUtilities/getWindowAncestor widget)]
               (when p

                 ;; A widget must only live in one parent container
                 (assert (or (= p x) (nil? x)))
                 
                 (add-dispose-listener p fun)))))))))))


;; Will automatically unsubscribe when the widget closes.
;; Returns the widget
(defn bind-widget-updater-sub [controller widget updater]
  (updater nil (get-local-state controller))
  (let [remover (subscribe controller updater)]
    (bind-on-dispose
     widget
     (fn []
       ;;(log-message "Remove watches for widget " widget)
       remover))
    widget))

;; Like the above function, but only forwards the new value if it is not nil.
;; Returns the widget
(defn bind-widget-updater [widget controller updater]
  (bind-widget-updater-sub
   controller
   widget
   (fn [oldv newv]
     (if (not (nil? newv))
       (updater oldv newv)))))
  
    


;;;;;;;;;;;;;;;;;;;;;;;;;; This file: Bindings for common swing controller.


(defn bind-label-text [this controller]
  (bind-widget-updater
   this controller
   (fn [old-value new-value]
     (invoke-soon (.setText this new-value)))))

;;;;;;;;;;;;;;;;;;;;;;; JLabel
(extend-type JLabel
  Bindable
  (bind [this controller]
    (bind-label-text this controller)))

(defn different-from-local-state [local-state value]
  (not= (deref local-state) value))






;;;;;;;;;;;;;;;;;;;;;;;;; Text
(defn- add-default-textfield-handlers [this controller local-state]
  (assert (Controller? controller))
  (assert (Atomic? local-state))
  (report-errors
   (let [update-model (fn []
                        (let [txt (invoke-soon (.getText this))]
                          (reset-async controller txt)))
         init-value (get-state controller)]
     (invoke-now
      (if (not (nil? init-value))
        (.setText this init-value)))
     (bind-widget-updater
       this
       controller
       (fn [old-value new-value]
         (invoke-now
          (when (different-from-local-state local-state new-value)
            (.setText this new-value)))))

     (if (instance? JTextField this)
       (.addActionListener
        this
        (proxy [ActionListener] []
          (actionPerformed [e]
            (update-model)))))

     (.addFocusListener
      this
      (proxy [FocusListener] []
        (focusLost [e]
          (update-model))
        (focusGained [e]
          (update-model))))
     init-value)))

(defn bind-deferred-text-widget [this controller]
  (add-default-textfield-handlers
   this controller (atom nil))
  this)

(defn bind-text-widget [this controller]
  (let [local-state (atom nil)
        init-value
        (add-default-textfield-handlers
         this controller local-state)]
    (report-errors
     (let [initialized (atom false)
           ud (fn []
                (swap!
                 initialized
                 (fn [init]
                   (let [txt (.getText this)]
                     (if (or init (= txt init-value) (not (empty? txt)))
                       (do (reset! local-state txt)
                           (reset-async controller txt)
                           true))))))]
       (.addDocumentListener
        (.getDocument this)
        (proxy [DocumentListener] []
          (changedUpdate [e]
            (ud))
          (removeUpdate [e]
            (ud))
          (insertUpdate [e]
            (ud)))))))
  this)

;;;;;;;;;;;;;;;;;;;;;;;;;;; JTextField
(extend-type JTextField
  Bindable
  (bind-deferred [this controller]
    (bind-deferred-text-widget this controller))
  (bind [this controller]
    (bind-text-widget this controller)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; JTextField
(extend-type JTextArea
  Bindable
  
  ;; Synchronizes model with text box, only when out of focus.
  (bind-deferred [this controller]
    (bind-deferred-text-widget this controller))

  ;; Synchronizes model with text box, immediately.
  (bind [this controller]
    (bind-text-widget this controller)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; JButton
(extend-type JButton
  Bindable
  (bind [this fun]
    (report-errors
     (assert (fn? fun))
     (.addActionListener this
                         (proxy [ActionListener] []
                           (actionPerformed [e]
                             (fun))))
     this))
  
  ;; Applies fun on the controller whenever
  ;; the button is pressed.
  (bind-apply [this controller fun]
    (report-errors
     (.addActionListener
      this
      (proxy [ActionListener] []
        (actionPerformed [e]
           (update-async
            controller fun)))))
    this))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; JCheckBox
(defn bind-boolean-button [checkbox controller]
  (bind-widget-updater
   checkbox controller
   (fn [old-value new-value]
     (invoke-soon
      (.setSelected checkbox new-value))))
  (.addActionListener
   checkbox
   (proxy [ActionListener] []
     (actionPerformed [e]
       (report-errors
        (invoke-soon
         (let [sel (.isSelected checkbox)]
           (reset-async controller sel)))))))
  checkbox)

(extend-type JCheckBox
  Bindable

  ;; Binds the boolean state of a checkbox to the boolean state of
  ;; of the controller.
  (bind [checkbox controller]
    (bind-boolean-button checkbox controller)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; JSpinner
(defn bind-integer-spinner [spinner controller]
  (.addChangeListener
   spinner
   (proxy [ChangeListener] []
     (stateChanged [e]
       (invoke-soon
        (let [v (.getValue spinner)]
          (reset-async controller v))))))
  (bind-widget-updater
   spinner controller
   (fn [old-value new-value]
     (.setValue spinner (Integer. new-value))))
  spinner)

(extend-type JSpinner
  Bindable
  (bind [spinner controller]
    (bind-integer-spinner
     spinner
     controller)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; JPanel (as a container)
(defn remove-widgets [widgets ks]
  ;; Close all controllers being removed
  (doseq [e (filter-map-by-key (fn [k]
                                 (contains? ks k))
                               widgets)]
    (close (:controller (second e))))

  ;; Keep the remaining ones.
  (filter-map-by-key
   (fn [k] (not (contains? ks k)))
   widgets))
        
       

;;; Base function for collection widgets.
(defn bind-widget-to-map [widget             ;; The widget
                          map-controller     ;; The controller for the map. Should control
                                             ;;   a key/value map of any kind.
                          widget-fun         ;; Called when new elements are added:
                                             ;;   [map-controller key value derived]
                          order-by-fun       ;; Once a new widget is created, it is
                                             ;;   assigned an ordering value from its
                                             ;;   current [key value] pair.
                          update-widget-fun] ;; Called with the sequence of all sorted
                                             ;;   widgets every time an element is added or
                                             ;;   removed.
  (let [widget-state (atom {})]
    (bind-widget-updater
     widget
     map-controller
     (fn [old-value new-value]
       (let [old-keys (set (keys (deref widget-state)))
             new-keys (set (keys new-value))]
         (report-errors
          ;; Only when there is a change in the key sets do we change the container.
          (when (not (= old-keys new-keys))
            (let [;; This is the updated widget set, which is swapped into the private state.
                  new-widgets
                  (swap! widget-state
                         (fn [widgets]
                                        ; Create widgets for the new keys
                           (loop [keys-to-add (clojure.set/difference new-keys old-keys)
                                  result (if (nil? widgets)
                                           {}
                                           (remove-widgets
                                            widgets
                                            (clojure.set/difference old-keys new-keys)))]
                             (if (empty? keys-to-add)
                               result
                               (recur (rest keys-to-add)
                                      (let [k (first keys-to-add)
                                            value (get new-value k)
                                            derived (derive-controller
                                                     map-controller
                                                     k)]
                                        (assoc result k
                                               {:controller derived
                                                :order-value (order-by-fun [k value])
                                                :widget (widget-fun map-controller
                                                                    k value derived)})))))))]
              ;; Refresh the contents of the container widget.
              (update-widget-fun
               (sort
                (fn [a b]
                  (compare (:order-value a)
                           (:order-value b)))
                (vals new-widgets)))))))))))

(defn bind-panel-to-map [panel
                         map-controller
                         widget-fun
                         order-by-fun]
  (bind-widget-to-map
   panel
   map-controller widget-fun order-by-fun
   (fn [sorted]
     (invoke-soon
      (.removeAll panel)
      (doseq [x sorted]
        (.add panel (:widget x)))
      (.revalidate panel)
      (.repaint panel)))))

(extend-type JPanel
  Bindable
  (bind-map [panel map-controller widget-fun order-by-fun]
    (bind-panel-to-map
     panel map-controller widget-fun order-by-fun)))







;;;;;;;;;;;;;;;;;;;;;; List widgets

(defn is-list-value [list]
  (and (or (if (contains? list :index)
             (let [v (:index list)]
               (or (number? v) (nil? v))))
           (if (contains? list :inds)
             (every? number? (:inds list))))
       (contains? list :items)))


(defn update-list-model [model items]
  (report-errors
   (.clear model)
   (doseq [item items]
     (.addElement model item))
   model))

(defn make-list-model [items]
  (update-list-model (DefaultListModel.) items))


;; (defn get-index-or-inds [x]
;;   (if (contains? x :index)
;;     (:index x)
;;     (:inds x)))

(defn ensure-selection-mode [list mode]
  (if (not= (.getSelectionMode list) mode)
    (.setSelectionMode list mode)))


;; Updates a model from the indices.
(defn make-model-index-updater [lock controller list]
  (fn []
    (future
      (with-lock lock
        (update-sync
         controller
         (fn [value]
           (if (contains? value :index)
             (assoc value :index (.getSelectedIndex list))
             (assoc value :inds (vec (.getSelectedIndices list))))))))))

(defn get-index-data [widget]
  (if (= (.getSelectionMode widget)
         ListSelectionModel/MULTIPLE_INTERVAL_SELECTION)
    (vec (.getSelectedIndices widget))
    (.getSelectedIndex widget)))

(defn get-index-mode [model]
  (if (contains? model :inds) :inds :index))

(defn set-index-data [model index-data]
  (assoc model (get-index-mode model) index-data))

(defn get-index-model-data [model]
  (get model (get-index-mode model)))

(defn bind-selection-listener [private-state lock list controller]
  (assert (instance? JList list))
  (assert (Controller? controller))
  (.addListSelectionListener
   list
   (proxy [ListSelectionListener] []
     (valueChanged [e]
       (if (not (deref lock))
         (let [index-data (get-index-data list)]
           (update-async
            controller
            (fn [value]
              (let [new-state (set-index-data value index-data)]
                (reset! private-state new-state)
                new-state)))))))))


(defn set-list-selection [list index-data]
  (if (coll? index-data)
    (do
      (ensure-selection-mode list ListSelectionModel/MULTIPLE_INTERVAL_SELECTION)
      (.setSelectedIndices list (int-array index-data)))
    (do
      (ensure-selection-mode list ListSelectionModel/SINGLE_SELECTION)
      (.setSelectedIndex
       list
       index-data))))

(defn make-init-fun []
  (let [state (atom false)]
    (fn []
      (let [x (deref state)]
        (reset! state true)
        x))))

(defn read-state-and-reset [state]
  (let [p (promise)]
    (swap! state (fn [x]
                   (deliver p x)
                   nil))
    (deref p)))

(defn read-state-and-update [private-state new-value]
  (let [p (promise)]
    (swap! private-state (fn [x]
                           (deliver p x)
                           new-value))
    (deref p)))

(defn make-list-updater [private-state lock  list controller]
  (assert (instance? JList list))
  (assert (Controller? controller))
  (let [list-model (DefaultListModel.)]
    (invoke-soon
     (.setModel list list-model))
    (fn [old-value new-value]
      (let  [last-state (read-state-and-update private-state new-value)]
        (when (not= last-state new-value)
          (invoke-soon
           (with-lock lock
             (when (not= (:items last-state) (:items new-value))
               (update-list-model list-model (:items new-value)))
             (set-list-selection list (get-index-model-data new-value)))))))))
         

(defn bind-list-updater [private-state lock list controller]
  (bind-widget-updater
   list controller
   (make-list-updater private-state lock list controller)))

(defn bind-list [list controller]
  (let [private-state (atom nil)
        lock (atom false)]
    (bind-selection-listener private-state lock list controller)
    (bind-list-updater private-state lock list controller)
    list))

(defn bind-list-old [list controller]
  (assert (Controller? controller))
  (let [lock (atom false)
        update-model (make-model-index-updater lock controller list)]
    (.addListSelectionListener
     list
     (proxy [ListSelectionListener] []
       (valueChanged [e]
         (update-model))))
    (bind-widget-updater
      list controller
      (fn [old-value new-value]
        (with-lock lock
          (report-errors
           (assert (or (nil? list)
                       (is-list-value new-value)))
           (let [items-updated (not= (:items old-value)
                                     (:items new-value))]
             (when items-updated
               (.setModel list (make-list-model (:items new-value))))

             ;; Set the index after model has been updated
             (if (contains? new-value :index)
;               (when (or items-updated (not= (:index old-value)
;                                             (:index new-value)))
               (do
                 (ensure-selection-mode list ListSelectionModel/SINGLE_SELECTION)
                 (.setSelectedIndex
                  list
                  (:index new-value)))
               ;(when (or items-updated (not= (:inds old-value) (:inds new-value)))
               (do
                 (ensure-selection-mode list ListSelectionModel/MULTIPLE_INTERVAL_SELECTION)
                 (.setSelectedIndices list (int-array (:inds new-value))))))
           )))))
    list)
       
             

(extend-type JList
  Bindable
  (bind [this list-ctrl]
    (bind-list this list-ctrl)))

(defn make-combo-box-model [items]
  (let [m (DefaultComboBoxModel.)]
    (doseq [i items]
      (.addElement m i))
    m))
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Combo box
(defn bind-combo-box [box list-ctrl]
  (let [lock (atom false)]
    (.addItemListener
     box
     (proxy [ItemListener] []
       (itemStateChanged [e]
         (future
           (report-errors
            (with-lock lock
              (let [index (.getSelectedIndex box)]
                (update-sync
                 list-ctrl
                 (fn [v]
                   (assoc v :index index))))))))))
    (bind-widget-updater
     box list-ctrl
     (fn [old-value new-value]
       (with-lock lock
         (invoke-soon
          (when (not= (:items old-value) (:items new-value))
            (.setModel box (make-combo-box-model (:items new-value))))
          (.setSelectedIndex box
                             (:index new-value)))))))
    box)

(extend-type JComboBox
  Bindable
  (bind [this list-ctrl]
    (bind-combo-box this list-ctrl)))
    



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; JRadioButton

(defn attach-close-button [frame]
  (let [b (JButton. "Close")]
    (.addActionListener
     b
     (proxy [ActionListener] []
       (actionPerformed [e]
         (.dispose frame))))
    (disp-widget b)))

(defn group-buttons-together [button-map]
  (let [group (ButtonGroup.)]
    (doseq [b (vals button-map)]
      (.add group b))
    group))
    

(defn bind-radio-buttons [button-map ; A map where every value is a JRadioButton
                          ctrl]
  (assert (every? (fn [x] (instance? javax.swing.JRadioButton x)) (vals button-map)))
  (doseq [[key button] button-map]
    (report-errors
     (.addActionListener
      button
      (proxy [ActionListener] []
        (actionPerformed [e]
          (if (.isSelected button)
            (update-sync ctrl (fn [x] key))))))))
  (let [group (group-buttons-together button-map)
        updater
        (fn [old-value new-value]          
          (if (not (nil? new-value))
            (invoke-soon
             (.clearSelection group)
             (if (contains? button-map new-value)
               (.setSelected (get button-map new-value) true)))))
        remover
        (subscribe
         ctrl updater)]
    (updater nil (get-local-state ctrl))
    (doseq [[key button] button-map]
      (bind-on-dispose button remover))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Toggle button
(defn bind-toggle-button [button labels controller]
  (bind-widget-updater
    button controller
    (fn [old-value new-value]
      (invoke-soon
       (.setText button
                 (if new-value
                   (second labels)
                   (first labels))))))
  (.addActionListener button
                      (proxy [ActionListener] []
                        (actionPerformed [e]
                          (update-async controller not))))
  button)













;;;;;;;;;;;;;;;;;;;;; DEMOS
(defn label-demo []
  (let [ctrl (make-controller {:a "Rulle"})
        sctrl (derive-controller ctrl :a)
        label (JLabel.)]
    (bind label sctrl)
    (disp-widget label)
    sctrl))

(defn text-demo [deferred?]
  (let [ctrl (make-controller {:a ""})
        sctrl (derive-controller ctrl :a)
        label (JLabel.)
        text (JTextField. 20)]
    (subscribe
     ctrl
     (fn [oldv newv]
       (log-message "New value: " newv)))
    (disp-widget (bind label sctrl))
    (disp-widget
     (if deferred?
       (bind-deferred text sctrl)
       (bind text sctrl)))
    sctrl))

(defn text-demo2 []
  (let [ctrl (make-controller "")
        text0 (JTextField. 20)
        text1 (JTextField. 20)]
    (disp-widget (bind-deferred text0 ctrl))
    (disp-widget (bind text1 ctrl))))

(defn counter-demo []
  (let [ctrl (make-controller "")
        f (JFrame.)]
    (disp-widget
     (bind-apply (JButton. "Increase by 1")
                 ctrl (fn [x] (str x "1"))))
    (disp-widget
     (bind (JLabel.) ctrl))
    ctrl))


(defn age-demo []
  (let [ctrl (make-controller {:name "Jonas" :age 28})
        name (derive-controller ctrl :name)
        age (float2str
             (constrained
              (derive-controller ctrl :age)
              (fn [age] (<= 0 age))))]
    (disp-widget
     (doto (JPanel.)
       (.add (bind (JTextField. 20) name))
       (.add (bind-deferred (JTextField. 20) age))
       (.add (bind (JLabel.) (labeled "ctrl" ctrl)))
       (.add (bind (JLabel.) (labeled "name" name)))
       (.add (bind (JLabel.) (labeled "age" age)))))))

(defn distributed-demo []
  (let [name (make-controller "Jonas")
        age (float2str
             (constrained
              (make-controller 0)
              (fn [age] (<= 0 age))))
        ctrl (compose {:name name
                       :age age})]
    (disp-widget
     (doto (JPanel.)
       (.add (bind (JTextField. 20) name))
       (.add (bind-deferred (JTextField. 20) age))
       (.add (bind (JLabel.) (labeled "ctrl" ctrl)))
       (.add (bind (JLabel.) (labeled "name" name)))
       (.add (bind (JLabel.) (labeled "age" age)))))))


(defn checkbox-demo []
  (let [ctrl (make-controller false)]
    (disp-widget
     (doto (JPanel.)
       (.add (bind (JLabel.) (derive-controller
                              ctrl
                              (fn [x]
                                (if x "CHECKED" "NOT CHECKED")))))
       (.add (bind (JCheckBox. "A checkbox") ctrl))))
    ctrl))

(defn button-demo []
  (let [b (bind (JButton. "Rulle")
                (fn []
                  (log-message "Performed!!!")))
        f (JFrame.)]
    (doto f
      (.add b)
      (.pack)
      (.show))))

(defn spinner-demo []
  (let [ctrl (make-controller 0)]
    (disp-widget
     (doto (JPanel.)
       (.add (bind (JSpinner.
                    (SpinnerNumberModel.
                     (Integer. 1)
                     (Integer. 0)
                     nil
                     (Integer. 1)))
                   ctrl))
       (.add (bind (JLabel.)
                   (int2str ctrl)))))))
                   

(defn text-area-demo []
  (let [ctrl (make-controller "")
        text (bind (JTextArea. 30 30) ctrl)
        label (bind (JLabel.) ctrl)]
    (disp-widget
     (doto (JPanel.)
       (.add text)
       (.add label)))))

(defn panel-demo []
  (let [ctrl (make-controller {1 "Rulle", 2 "Mjao"})
        panel (JPanel.)]
    (bind-map
     panel
     ctrl
     (fn [map-ctrl key value value-ctrl]
       (bind (JLabel.) value-ctrl))
     first)
    (disp-widget panel)

    ;; Functions to add new keys and remove keys
    [(fn [key value]
       (update-sync
        ctrl
        (fn [x]
          (assoc x key value))))
     (fn [key]
       (update-sync
        ctrl
        (fn [x] (dissoc x key))))]))

(defn list-demo-single []
  (let [model (atom {:index 1 :items ["Rulle" "Katja" "Havsan"]})
        ctrl (make-controller model)
        label (bind (JLabel. "")
                    (derive-controller ctrl str))]
    (disp-widget label)
    (attach-close-button (disp-widget
                          (bind (JList.)
                                ctrl)))
    model))

(defn list-demo-multi []
  (let [model (atom {:inds [] :items ["Rulle" "Katja" "Havsan"]})
        ctrl (make-controller model)
        label (bind (JLabel. "")
                    (derive-controller ctrl str))]
    (disp-widget label)
    (disp-widget
     (bind (JList.)
           ctrl))
    model))

(defn combo-box-demo []
  (let [model (atom {:index 0 :items ["Rulle" "Katja" "Havsan"]})
        ctrl (make-controller model)]
    (attach-close-button (disp-widget (bind (JComboBox.) ctrl)))
    (attach-close-button (disp-widget (bind (JLabel. "") (derive-controller ctrl str))))
    model))

(defn radio-button-demo []
  (let [close-button (JButton. "Close all")
        model (atom :rulle)
        ctrl (make-controller model)
        b0 (JRadioButton. "Rulle")
        b1 (JRadioButton. "Havsan")]
    (bind-radio-buttons {:rulle b0, :havsan b1} ctrl)
    
    (let [frames [(disp-widget
                   (bind (JLabel. "")
                         (derive-controller ctrl
                                            (fn [x]
                                              (str "Current selection: " x)))))
                  (disp-widget b0)
                  (disp-widget b1)]]
      (.addActionListener close-button
                          (proxy [ActionListener] []
                            (actionPerformed [e]
                              (doseq [x frames]
                                (.dispose x)))))
      (disp-widget close-button))))

(defn toggle-button-demo []
  (let [model (atom false)
        ctrl (make-controller model)]
    (attach-close-button
     (disp-widget (bind-toggle-button (JButton.) ["false" "true"] ctrl)))))

(defn bind-enabled [widget controller]
  (bind-widget-updater
   widget controller
   (fn [old-value new-value]
     (.setEnabled widget (if new-value true false)))))




;;;; Bindings
(defrecord2 WidgetText [widget])

(defn get-text [widget]
  (WidgetText. widget))

(extend-type WidgetText
  Bindable
  (bind [widget-text controller]
    (subscribe-and-update
     controller
     (fn [old-value new-value]
       (.setText (:widget widget-text) new-value)))))
