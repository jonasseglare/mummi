(ns mummi.signe.process
  (:require [clojure.core.async :refer [chan sliding-buffer close! <!! >!! thread]])
  (:require [mummi.debug :refer [value-of]])
  (:require [mummi.async :refer [poll!]])
  (:require [mummi.common :refer [vecseq?]])
  (:require [mummi.signe.controller :refer :all]))

(defn- launch-process [map-controller key start-process]
  (future
    (let [root-controller (get-root-controller map-controller)
          controller (derive-controller map-controller key)]
      (start-process
       {:key key
        :controller controller
        :input-channel (make-output-channel controller)
        :root-controller root-controller
        :root-input-channel (make-output-channel root-controller)}))))
     
      
;; start-process is this
;; kind of function:
;; (start-process process-controller input-root-channel output-root-update-channel)
;; Main function to run a process.
(defn run [map-controller start-process]
  (let [upd (fn [oldv newv]
              (let [new-keys (clojure.set/difference
                              (set (keys newv))
                              (set (keys oldv)))]
                (doseq [key new-keys]
                  (launch-process map-controller
                                  key
                                  start-process))))]
    (subscribe map-controller upd)
    (upd nil (get-state map-controller))))


(defn- make-updater [x]
  (cond
    (fn? x) x
    (nil? x) identity
    :default (fn [y] x)))

(defn- fix-result-v [x]
  (if (not (nil? x))
    (if (vecseq? x)
      (map make-updater x)
      (fix-result-v [x x]))))

;;; Main function to run a process:
;;; map-controller is a controller for all processes
;;; (fun root-state local-state) -> [root-updater local-updater] or nil
(defn run-apply [map-controller fun]
  (run map-controller
    (fn [process-data]
      (let [to-do (chan (sliding-buffer 1))
            run-the-loop
            (fn []
              (loop []
                (let [root-state (get-state (:root-controller process-data))
                      local-state (get-local-state (:controller process-data))]
                  (if (not (or (nil? root-state)
                               (nil? local-state)))
                    (let [results (fix-result-v (fun root-state local-state))]
                      (if (not results)
                        (>!! to-do true)                        
                        (let [[root-upd upd] results]
                          (update-sync (:controller process-data) upd)
                          (update-sync (:root-controller process-data) root-upd)
                          (recur))))))))]
        ;; Restart the loop if it was parked.
        (subscribe (:controller process-data)
                   (fn [oldv newv]
                     (if (poll! to-do)
                       (run-the-loop))))

        ;; Start the loop
        (run-the-loop)))))
              








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn demo-start-ticker [data]
  (let [counter (:controller data)]
    (loop []
      (let [state (get-state (:root-controller data))]
        (Thread/sleep (:delay state))
        (let [counter-state (get-state counter)]
          (when (< counter-state 30)
            (println "TICK! " counter-state)
            (update-async counter inc)
            (recur)))))))

(defn run-ticker-demo []
  (let [ctrl (make-controller
              {:delay 1000
               :processes {:rulle 0 :mjao 10}})]
    (run (derive-controller ctrl :processes) demo-start-ticker)
    ctrl))

(defn run-ticker-demo2 []
  (let [ctrl (make-controller
              {:delay 1000
               :processes {:rulle 0 :mjao 10}})]
    (run-apply
     (derive-controller ctrl :processes)
     (fn [root-state state]
       (println "Tick... " state)
       (Thread/sleep (:delay root-state))
       (if (> state 0)
         [identity dec])))
    ctrl))
