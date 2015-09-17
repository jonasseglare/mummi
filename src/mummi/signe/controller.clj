(ns mummi.signe.controller
  (:require [clojure.core.async :refer [chan sliding-buffer close! thread <!! >!! go >! <!]])
  (:require [mummi.common :as common :refer [defrecord2 defprotocol2 make-symbol]])
  (:require [mummi.atomic :refer [atom? watch-atoms-until-any-nil touch
                                  watch-atom-until-nil a-swap a-reset Atomic?
                                  ref? atom?]])
  (:require [mummi.async :refer [poll!]])
  (:require [mummi.debug :refer [value-of report-errors]])
  (:require [clojure.walk :refer [postwalk]])
  (:require [mummi.log :refer [log-message]]))


;;; REMEMBER:
;;;  * Close the controller when the underlying model is no longer used, or
;;;    detach it.




(defprotocol2 Controller
  (syncable? [this])
  (update-sync [this fun])

  ;; Get the state from the root.
  (get-state [this])

  ;; Get the state from the local cache.
  (get-local-model [this])

  (get-root-controller [this])
  
  ;; Subscribe to updates until it returns nil
  (subscribe-nil [this fun])
  
  (update-async [this fun])
  (close [this]))

(defn subscribe [ctrl fun]
  (let [remover
        (subscribe-nil
         ctrl
         (fn [oldv newv]
           (fun oldv newv)
           true))]
    (assert (fn? remover))
    remover))

(defn subscribe-and-update
  ([ctrl fun] (subscribe-and-update ctrl fun nil))
  ([ctrl fun oldv]
   (subscribe ctrl fun)
   (fun oldv (get-state ctrl))
   ctrl))


(defn get-local-state [x]
  (deref (get-local-model x)))

(defn get-root-model [x]
  (if-let [ctrl (get-root-controller x)]
    (get-local-model ctrl)))

(defprotocol2 Bindable
  ;; Changes in the widget are applied later.
  ;; Userful for textboxes.
  (bind-deferred [widget controller])

  ;; Changes in the widget are applied
  ;; immediately.
  (bind [widget controller])

  ;; Whenever an event related
  ;; to the widget occurs,
  ;; the controller is updated
  ;; with fun.
  (bind-apply [widget controller fun])

  ;; Binds a widget to a map. widget-fun accepts
  ;;
  ;; (widget-fun map-controller key value value-controller)
  ;;
  ;; and should return a new widget.
  (bind-map [widget map-controller widget-fun order-by-fun-kv]))


(defn reset-sync [controller value]
  (update-sync controller
               (fn [x] value)))

(defn reset-async [controller value]
  (update-async controller
                (fn [x] value)))


(defrecord2 RootController
  [private-model
   now-channel
   active?])

;; Reads all the changes from the later-chan and applies them to the model.
(defn- get-functions-to-apply [channel]
  (loop [result []]
    (let [fun (poll! channel)]
      (if fun
        (recur (conj result fun))
        result))))

(defn- get-all-functions-to-apply [controller]
  (get-functions-to-apply (:later-channel controller)))

(defn- apply-functions [funs x]
  (loop [remain funs
         result x]
    (if (empty? remain)
      result
      (recur (rest remain)
             ((first remain) result)))))

;; Whenever something is put on the now-channel, a full update of the state is
;; performed. This can be more efficient than update-syncping directly.
(defn- start-updater [controller]
  (thread
    (report-errors
     (loop []
       (when-let [fun (<!! (:now-channel controller))]
         (a-swap (:private-model controller)
                (fn [x]
                  (apply-functions
                   (concat [fun]
                           (get-functions-to-apply
                            (:now-channel controller))) x)))
         (recur)))))
  controller)


(defn- attach-main-watch [controller]
  (assert (RootController? controller))
  (subscribe
   controller
   (fn [oldv newv]
     (when (and (nil? newv) (deref (:active? controller)))
       (a-reset (:active? controller) nil)
       (close! (:now-channel controller))
       (close! (a-reset (:updaters controller) nil)))))
   controller)



(defn was-changed-swap [x fun]
  (let [p (promise)]
    (a-swap x
            (fn [y]
              (let [z (fun y)]
                (deliver p (not= z y))
                z)))
    (deref p)))

(defn touch-on-false [local-state value]
  (if (not value)
    (touch local-state))
  value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Root controller
(extend-type RootController
  Controller
  (update-sync [this fun]
    (assert (fn? fun))
    (was-changed-swap (:private-model this) fun))
  (get-root-controller [this] this)
  (get-state [this]
    (deref (:private-model this)))
  (get-local-model [this]
    (:private-model this))
  (update-async [this fun]
    (let [p (promise)]
      (>!! (:now-channel this)
           (fn [x]
             (let [y (fun x)]
               (deliver p (not= y x))
               y)))
      p))
  (syncable? [this]
    (ref? (:private-model this)))
  (subscribe-nil [this fun]
    (watch-atoms-until-any-nil
     [(:private-model this) (:active? this)]
     fun))
  (close [this]
    (a-reset (:active? this) nil)
    (deref (:private-model this))))

;; Make a root controller.
(defn make-controller
  ([model-atom] (make-controller model-atom 1024))
  ([model-atom buflen]
   (if (Controller? model-atom)
     model-atom
     (start-updater
      (attach-main-watch
       (RootController.
        (if (Atomic? model-atom)
          model-atom
          (ref model-atom))
        (chan (sliding-buffer buflen))
        (atom true)))))))














;;;;;;; Private for subcontroller
(defrecord2 DerivedController [parent private-model get-fun set-fun])

;; Used by a derived controller to
;; listen to the parent until the parent
;; becomes nil.
(defn- attach-to-parent [controller]
  (assert (DerivedController? controller))
  (assert (fn? (:set-fun controller)))
  (assert (fn? (:get-fun controller)))
  (assert (Atomic? (:private-model controller)))
  (let [gf (:get-fun controller)]
    (subscribe-nil
     (:parent controller)
     (fn [old-valuep new-valuep]
       (report-errors
        (if (nil? new-valuep)
          (a-reset (:private-model controller) nil)
          (if (not= :unsubscribe
                    (if (not (= old-valuep new-valuep))
                      (let [old-value (gf old-valuep)
                            new-value (gf new-valuep)]
                        (if (not (= old-value new-value))
                          (if (nil? (a-swap (:private-model controller)
                                           (fn [x] (if (not (nil? x))
                                                     new-value))))
                            :unsubscribe)))))
            true))))))
  controller)

(defn make-parent-updater [controller fun]
  (fn [x]
    (let [transformed (fun ((:get-fun controller) x))
          new-parent-value
          ((:set-fun controller) x transformed)
          the-same (= new-parent-value x)]
      new-parent-value)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Derived controller
;; Derive a controller from another controller.
(defn derive-controller
  ([parent get-state-or-key]
   (if (fn? get-state-or-key)
     (derive-controller parent get-state-or-key (fn [dst x] dst))
     (derive-controller parent
               (fn [x] (get x get-state-or-key))
               (fn [dst value] (assoc dst get-state-or-key value)))))
  ([parent get-fun set-fun]
   (attach-to-parent
    (DerivedController.
     parent
     (ref (get-fun (get-state parent)))
     get-fun
     set-fun))))


(extend-type DerivedController
  Controller
  (update-sync [this fun]
    (let [p (update-sync (:parent this)
                         (make-parent-updater this fun))]
      (if (not p)
        (touch (:private-model this)))
      p))
  (syncable? [this]
    (syncable? (:parent this)))
  (get-root-controller [this]
    (get-root-controller (:parent this)))
  (get-state [this]
    ((:get-fun this) (get-state (:parent this))))
  (get-local-model [this]
    (:private-model this))
  (subscribe-nil [this fun]
    (watch-atom-until-nil (:private-model this) fun))
  (update-async [this fun]
    (let [p (update-async (:parent this)
                          (make-parent-updater this fun))]
      (future
        (if (not (deref p))
          (touch (:private-model this))))
      p))
  (close [this]
    (a-reset (:private-model this) nil)))











;;;;; Private of composed controller
(defrecord2 ComposedController [part-map private-model now-channel])


(defn start-part-map-updaters [controller]
  (assert (ComposedController? controller))
  (doseq [[key ctrl] (:part-map controller)]
    (subscribe-nil
     ctrl
     (fn [old-value new-value]
       (if (nil? new-value)
         (a-reset (:part-map controller) nil)
         (if (= old-value new-value)
           (deref (:private-model controller))
           (a-swap (:private-model controller)
                   (fn [old-part-map]
                     (assoc old-part-map
                            key new-value))))))))
  controller)

(defn syncable-controller? [x]
  (if (Controller? x)
    (syncable? x)))


;; Compose a controller from a map of other controllers.
;; All controllers must be syncable.
(defn compose [part-map]
  (assert (map? part-map))
  (assert (every? syncable-controller? (vals part-map)))
  (start-updater
   (start-part-map-updaters
    (ComposedController.
     part-map
     (ref
      (zipmap
       (keys part-map)
       (map get-state
            (vals part-map))))
     (chan (sliding-buffer 1024))))))

  (extend-type ComposedController
    Controller
    (syncable? [this] true)
    (update-sync [this fun]
      (dosync
       (let [input (get-state this)
             output (fun input)]
         (doseq [key (keys output)]
           (reset-sync
            (get (:part-map this) key)
            (get output key)))
         (not= input output))))
    (get-state [this]
      (dosync
       (zipmap
        (keys (:part-map this))
        (map get-state
             (vals (:part-map this))))))
    (get-local-model [this]
      (:private-model this))
    (get-root-controller [this] nil)
    (subscribe-nil [this fun]
      (watch-atom-until-nil (:private-model this) fun))
    (update-async [this fun]
      (let [p (promise)]
        (future
          (deliver p (update-sync this fun)))
        p))
    (close [this]
      (a-reset (:private-model this) nil)))

(defn make-output-channel [controller]
  (assert (Controller? controller))
  (let [c (chan (sliding-buffer 1))]
    (subscribe controller
               (fn [oldv newv]
                 (if (not= oldv newv)
                   (go
                     (>! c newv)))))
    c))

(defn get-root-channel [ctrl]
  (:now-channel (get-root-controller ctrl)))

