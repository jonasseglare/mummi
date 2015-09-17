(ns mummi.queue)

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn queue? [x]
  (instance? clojure.lang.PersistentQueue x))
  
