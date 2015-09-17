(ns mummi.text)

(defn string-to-integers [s]
  (map (fn [x] (Integer. x)) (clojure.string/split s #" ")))
