(ns mummi.log
  (:require [clojure.core.async :refer [chan thread <!! >!! sliding-buffer]]))

(defn make-log-channel
  ([] (make-log-channel 30))
  ([n]
   (chan (sliding-buffer n))))

; Starts a logger and returns log
(defn start-default-logger [log & args]
  (let [logger (if (nil? args)
                 println
                 (first args))]
    (thread
      (loop []
        (let [x (<!! log)]
          (if (nil? x) nil
            (do (logger x)
                (recur))))))
    log))

(defn make-async-logger []
  (let [log (make-log-channel)]
    (start-default-logger log)
    (fn [message] (thread (>!! log message)) nil)))

(let [current-logger (atom nil)]
  (defn ensure-logger []
    (if (nil? (deref current-logger))
      (reset! current-logger (make-async-logger))))

  (defn get-current-logger []
    (deref current-logger))
  
  (defn log-message [& args]
    (ensure-logger)
    (let [logger (deref current-logger)]
      (do
        (assert (fn? logger))
        (logger (apply str args)))))

  (defn output [ & args]
    (apply log-message args))

  (defn set-current-logger [new-logger]
    (let [old-logger (deref current-logger)]
      (reset! current-logger new-logger)
      old-logger)))

(defn use-async-logger []
  (set-current-logger (make-async-logger)))


(defmacro with-logger [log-fun-vec & calls]
  (let [old-logger (gensym)
        result (gensym)]
    `(let [~old-logger (set-current-logger ~(if (empty? log-fun-vec) nil
                                                (first log-fun-vec)))
           ~result (do ~@calls)]
       (set-current-logger ~old-logger)
       ~result)))

;;; Demo
(comment

  (do
    (log-message "Rulle")
    (with-logger [(fn [mes] (println "MY CUSTOM LOGGER: " mes))]
      (log-message "Rudolf!!!"))

    (log-message "Mjao"))



  )
     
