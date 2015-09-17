(ns mummi.timeout
  (:require [mummi.quantity :refer :all]))

;; Returns nil if the the (do ~@args) doesn't finish withing timeout
;;  (with-timeout [(make-seconds 1)] 119) will return 119
;;  (with-timeout [(make-seconds 1)] (Thread/sleep 2000) 119) will return nil
;;  (with-timeout [(make-seconds 1) :rulle] (Thread/sleep 2000) 119)
;;
;;
;; Use this to wrap larger operations, in order to ensure that the
;; driver is not blocked.
(defmacro with-timeout [timeout-onerror & args]
  (assert (vector? timeout-onerror))
  (let [arg-count (count timeout-onerror)
        timeout-x (first timeout-onerror)
        on-error (second timeout-onerror)]
    (let [sym (gensym)
          T (gensym)]
      `(let [~sym (future ~@args)]
         (deref ~sym
                (let [~T ~timeout-x]
                  (get-milliseconds (if (number? ~T) (make-seconds ~T) ~T)))
                ~on-error)))))
