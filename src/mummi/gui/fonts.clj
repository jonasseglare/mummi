(ns mummi.gui.fonts
  (:import [java.awt Font]))

(defn make-monospaced
  ([pt] (Font. "monospaced" Font/PLAIN pt))
  ([] (make-monospaced 12)))

(defn make-font [f]
  (cond
    (= f :monospaced) (make-monospaced)
    (number? f) (make-monospaced f)
    :default (make-monospaced)))
