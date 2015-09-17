(ns mummi.env)

; Environment-related.

(defn get-home []
  (System/getProperty "user.home"))
