(ns mummi.gui.paintable
  (:import [javax.swing JPanel JFrame])
  (:import [java.awt Color BasicStroke])
  (:require [mummi.log :refer [log-message]]))

; A panel that listens to a state and will repaint that state whenever it changes.
;
; painter-function takes a Graphics2d and the new state
; state-atom is the state to be listened to
(defn make-paintable-jpanel [painter-function state-atom]
  (assert (fn? painter-function))
  (let [last-known-state (atom (deref state-atom))
        panel (proxy [JPanel] []
                (paintComponent [g2]
                  (proxy-super paintComponent g2)
                  (painter-function g2 (deref last-known-state))))]
    (add-watch
     state-atom
     nil
     (fn [k r ov nv]
       (reset! last-known-state
               nv)
       (javax.swing.SwingUtilities/invokeLater #(.repaint panel))))
    panel))

;

(defn paint-demo []
  (let [r 100
        c (+ r 30)
        state (atom 0)
        painter (fn [g2 state]
                  (let [angle (* 0.03 state)
                        x (+ c (* r (Math/cos angle)))
                        y (+ c (* r (Math/sin angle)))]
                    (.setColor g2 (Color. 240 0 70))
                    (.setStroke g2 (BasicStroke. 7 BasicStroke/CAP_ROUND
                                                 BasicStroke/JOIN_ROUND))
                    (.drawLine g2 c c x y)))
        panel (make-paintable-jpanel painter state)
        frame (JFrame. "Demo")]
    (.setDoubleBuffered panel true)
    (.setBackground panel (Color. 255 255 255))
    (future (loop []
              (Thread/sleep 300)
              (swap! state (fn [x] (+ x 1)))
              (recur)))
    (doto frame
      (.setSize 400 300)
      (.setContentPane panel)
      (.show))
    state))

    
              
              
        
     
      
          
      
