(ns mummi.swinglog
  (:import [javax.swing JTextArea JFrame JScrollPane])
  (:require [mummi.gui.fonts :refer [make-monospaced]]))


;;;;;;;;;;;;;;;;; WARNING : Don't include in builds!

(comment
  (defn make-log-area []
    (doto (JTextArea. 24 80)
      (.setEditable false)
      (.setFont (make-monospaced))))

  (let [log-area (make-log-area)
        log-window (doto (JFrame. "Log")
                     (.add (JScrollPane. log-area))
                     (.pack))]
    (defn add [& args]
      (doseq [arg args]
        (.append log-area (str arg)))
      (.append log-area "\n")
      (if (not (.isVisible log-window))
        (.show log-window)))))
