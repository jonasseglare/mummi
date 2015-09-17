(ns mummi.access)

(defn make-getter [ks]
  (fn [x]
    (get-in x ks)))

(defn make-setter [ks]
  (fn [x y]
    (assoc-in x ks y)))
