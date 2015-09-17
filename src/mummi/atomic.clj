(ns mummi.atomic
  (:require [mummi.log :refer [log-message]])
  (:require [mummi.common :refer [with-field defprotocol2]])
  (:require [mummi.queue :refer :all])
  (:require [clojure.core.async :refer [thread <!! >!! chan sliding-buffer close!]])
  (:require [mummi.debug :refer [report-errors value-of with-report-errors]]))

;; Mainly for atoms and refs.
;; Since ref can be seen as a generalization of
;; an atom, it makes sense to have a common
;; protocol for both.
(defprotocol2 Atomic
  (a-swap [this fun])
  (a-reset [this fun]))

(extend-type clojure.lang.Ref
  Atomic
  (a-swap [this fun]
    (dosync
     (alter this fun)))
  (a-reset [this value]
    (dosync
     (ref-set this value))))

(extend-type clojure.lang.Atom
  Atomic
  (a-swap [this fun]
    (swap! this fun))
  (a-reset [this value]
    (reset! this value)))

(defn atom? [x]
  (instance? clojure.lang.Atom x))

(defn ref? [x]
  (instance? clojure.lang.Ref x))


;;;;;;;; Functions to manage processes that operate on atoms.
;;;;;;;; Processes are attached to atoms by appending their id's to
;;;;;;;; the :processes set of an atom.

(comment ; How bindings work
(do (future (binding [A 0] (println A) (binding [A 1] (println A)) (println A)))
		  (binding [A 119] (println "And here A is " A) (future (println "In the future it is " A)))))

(with-report-errors

  (defn watch-once
    ([stateref wkey fun]
     (add-watch stateref wkey
                (fn [wkey stateref old-value new-value]
                  (remove-watch stateref wkey)               
                  (fun stateref wkey old-value new-value))))
    ([stateref fun3]
     (watch-once stateref
                 (gensym)
                 (fn [stateref wkey old-value new-value]
                   (fun3 stateref old-value new-value)))))

  ;; Watches the atom until either a new value is nil,
  ;; or the function returns nil.
  (defn watch-atom-until-nil [dst fun]
    (let [sym (gensym)
          remover (fn [] (remove-watch dst sym))]
      (add-watch dst
                 sym
                 (fn [wkey0 ref0 old-value new-value]
                   (when (or (nil? new-value) (nil? (fun old-value new-value)))
                     (remover))))
      remover))

  (defn watch-atoms-until-any-nil [targets fun]
    (let [ref-key-pairs (map
                         (fn [a b]
                           [a b])
                         targets
                         (repeatedly (count targets) gensym))
          remove-watches (fn []
                           (doseq [[aref wkey] ref-key-pairs]
                             (remove-watch aref wkey)))
          [r0 k0] (first ref-key-pairs)]
      (add-watch r0 k0
                 (fn [wkey0 ref0 old-value new-value]
                   (if (nil? new-value)
                     (do (remove-watches)
                         (fun nil nil))
                     (if (nil? (fun old-value new-value))
                       (remove-watches)))))

      (doseq [[aref wkey] (rest ref-key-pairs)]
        (add-watch aref wkey
                   (fn [wkey0 ref0 old-value new-value]
                     (when (nil? new-value)
                       (fun nil nil)
                       (remove-watches)))))
      remove-watches))

  (defn wait-for-update [state-atom]
    (let [next-state (promise)]
      (watch-once state-atom (fn [x old-state new-state]
                               (deliver next-state new-state)))
      (deref next-state)))
  
  


  (defn touch [x]
    (assert (Atomic? x))
    (a-swap x (fn [y] y)))

  (defn attachable? [x]
    (if (Atomic? x)
      (attachable? (deref x))
      (map? x)))

  (defn get-process-set [x]
    (if-let [p (:processes x)]
      p
      #{}))

  (defn attach-process [x process-id]
    (assoc x :processes
           (conj (get-process-set x)
                 process-id)))

  (defn has-process? [x process-id]
    (contains?
     (get-process-set x)
     process-id))

  (defn detach-process [x process-id]
    (if (Atomic? x)
      (a-swap x (fn [y] detach-process y process-id))
      (assoc x :processes
             (disj (get-process-set x)
                   process-id))))

  (defn detach-all [x]
    (if (Atomic? x)
      (a-swap x detach-all)
      (assoc x :processes nil)))

  (defn make-generic-fn [base-fn state-atom process-id]
    (fn
      ([state] (base-fn state process-id))
      ([] (base-fn (deref state-atom) process-id))))
  
  (defn make-attached?-fn [state-atom process-id]
    (make-generic-fn has-process? state-atom process-id))

  (defn make-detach-from-fn [state-atom process-id]
    (make-generic-fn detach-process state-atom process-id))


                                        ;(def ^:dynamic attached?)
                                        ;(def ^:dynamic detach-from)

                                        ;(defmacro with-state [state & exprs]
                                        ; Startar en tråd asynkront och returnerar en symbol som associeras
                                        ; med denna tråd.
                                        ;
                                        ; I en future körs sedan en swap som lägger till denna symbol i
                                        ; ett set av :processes.
                                        ;
                                        ; Funktionerna   - (detach)    : på aktuellt tillstånd
                                        ;                - (detach x)  : returnerar ett nytt tillstånd där x är d.
                                        ;                - (attached?)
                                        ;                - (attached? x)
  (defmacro with-state [state-atom-and-process-sym & exprs]
    (let [[state-atom0 process-id] state-atom-and-process-sym
          state-atom (gensym)
          temp (gensym)]
      `(report-errors
        (let [~state-atom ~state-atom0
              ~process-id (gensym)]
          (future
            (do
              (a-swap ~state-atom (fn [~temp]
                                   (attach-process ~temp ~process-id)))
              ~@exprs))
          ~process-id))))


  ;; Starts a new thread in which 'exprs' are looped
  ;; as long as the thread is attached to state-atom.
  (defmacro loop-while-attached [state-atom0 & exprs]
    `(let [state-atom# ~state-atom0]
       (with-state
         [state-atom# process-id#]
         (loop []
           (when (has-process? (deref state-atom#) process-id#)
             ~@exprs
             (recur))))))
  

  ;; Takes an atom and a value and compares the value of the atom with the new value.
  ;; If the new value is different, it returns true and updates the atom with
  ;; that value. Otherwise, it returns false.
  (defn was-updated? [state x]
    (assert (Atomic? state))
    (if (= (deref state) x)
      false
      (do (a-reset state x)
          true)))

                                        ; Monitors state-atom:
                                        ;  * cmp-fun: Takes a new and an old state, and returns a positive value if state needs update.
                                        ;  * transform-fun: Takes old and new state and returns an updated state.
  (defn start-monitor [state-atom cmp-fun transform-fun]
    (assert (Atomic? state-atom))
    (assert (fn? cmp-fun))
    (let [wkey (gensym)
          process-id (gensym)]
      (future
        (report-errors
         (a-swap state-atom (fn [x] (attach-process x process-id))))
        (let [last-state (atom (deref state-atom))
              handler
              (fn [key reference old-state new-state]
                (if (has-process? new-state process-id)
                  (if (was-updated? last-state new-state)
                    (if-let [cmp-result (cmp-fun old-state new-state)]
                      (future
                        (a-swap
                         state-atom
                         (fn [state]
                           (transform-fun old-state state))))))
                  (remove-watch state-atom wkey)))]
          (add-watch
           state-atom wkey
           handler)))
      process-id))


  ;; Returns a channel that will receive
  ;; new states whenever state-atom changes.
  ;; It will close the channel and stop pushing once
  ;; process-id is detached.
  ;;
  ;; Returns a channel.
  (defn start-watch
    ([state-atom] (start-watch state-atom 300))
    ([state-atom bufsize]
     (report-errors
      (let [wkey (gensym)
            c (chan (sliding-buffer bufsize))
            rwatch (fn []
                     (remove-watch state-atom wkey))]
        (future
          (add-watch state-atom
                     wkey
                     (fn [reference key old-value new-value]
                       (report-errors
                        (when (not (= new-value old-value))
                          (when (not (>!! c new-value))
                            (rwatch)))))))
        c))))


  (defn try-extract-output [state-atom extractor]
    (let [output (atom nil)]
      (a-swap state-atom (fn [x]
                          (if-let [result (extractor x)]
                            (do (a-reset output result)
                                (first result))
                            x)))
      (deref output)))

  (defn call-destructor [destructor internal-state]
    (destructor internal-state)
    nil)

  (defn start-atomic-worker [state-atom
                             attached?
                             extractor
                             processor
                             initial-state
                             destructor]
    (future
      (report-errors
       (assert (not (nil? initial-state)))
       (assert (fn? attached?))
       (assert (fn? extractor))
       (assert (fn? processor))
       (assert (fn? destructor))
       (let [c (start-watch state-atom)
             swap-loop-fun
             (fn [internal-state0] ;; returns next internal state, or nil if none.
               (report-errors
                (loop [internal-state internal-state0]

                  ;; Quit through signal from the processor?
                  (if (not (nil? internal-state))
                    
                    ;; Try to extract from the latest state
                    (if-let [state-and-output (try-extract-output state-atom extractor)]
                      (let [[state output] state-and-output]
                        ;; Quit through process closing?
                        (if (attached? state)
                          (recur (processor state output internal-state))
                          (call-destructor destructor internal-state)))
                      internal-state)))))]
         (>!! c (deref state-atom))
         
         (loop [internal-state (swap-loop-fun initial-state)]
           (when (not (nil? internal-state))
             (let [state (<!! c)]
               (if (attached? state)
                 (recur (swap-loop-fun internal-state))
                 (call-destructor destructor internal-state)))))
         (close! c)))))



  ;; state-atom: An atom or a ref.
  ;; get-processes-and-context: returns a map where every key maps to a context
  ;; start-process: Takes a state-atom, a proc-key and the context.
  (defn start-process-manager [state-atom
                               get-processes-and-contexts
                               start-process]
    (assert (Atomic? state-atom))
    (assert (fn? get-processes-and-contexts))
    (assert (fn? start-process))
    (let [key (gensym)]
      (add-watch
       state-atom
       key
       (fn [key state-atom old-state new-state]
         (if (nil? new-state)
           (remove-watch state-atom key)
           (let [old-procs (get-processes-and-contexts old-state)
                 new-procs (get-processes-and-contexts new-state)]
             (assert (map? old-procs))
             (assert (map? new-procs))
             (let [new-keys (clojure.set/difference
                             (set (keys new-procs))
                             (set (keys old-procs)))]
               (doseq [proc-key new-keys]
                 (future (start-process state-atom proc-key (get new-procs proc-key))))))))))
    state-atom)

  
  
); with-report-errors
  


  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Queue management:

                                        ;(defn put [state-atom ]


  ;; (defn read-queue [data ks]
  ;;   (let [q (get-in data ks)]
  ;;     (cond
  ;;      (nil? q) [:closed nil]
  ;;      (empty? q) [:empty q]
  ;;      :default [:value (peek q)])))
  

                                        ; Pop an element from a queue in a data structure.
                                        ; Blocks until either:
                                        ;  * An element was popped
                                        ;  * The current process was detached
                                        ;  * The queue was closed or is nil
                                        ; Returns nil if the queue was closed or if the
                                        ; process was detached.
  ;; (defn pop-queue [state-atom process-id ks]
  ;;   (assert (Atomic? state-atom))
  ;;   (let [wkey (gensym)
  ;;         result (promise)]
  ;;     (add-watch state-atom
  ;;                wkey
  ;;                (fn [key reference old-state new-state]
  
  



  (comment

    (def state (atom #{}))

    (start-process-manager state
                           (fn [s]
                             (zipmap
                              s s))
                           (fn [s key context]
                             (log-message "start process")
                             (future
                               (report-errors
                                (loop []
                                  (if (contains?
                                       (deref s)
                                       key)
                                    (do (log-message "Process for " key)
                                        (Thread/sleep 500)
                                        (recur))
                                    (log-message "Quitting process for " key)))))))

    (a-swap state (fn [x] (conj x (gensym))))
    (a-reset state #{})
    












  (defn queue-reader [data-to-list]
    (let [state (atom {:data data-to-list})
          process-id (start-atomic-worker
                      state
                      (fn [x]
                        (if (empty? (:data x))
                          nil
                          [(update-in x [:data] rest)
                           (first (:data x))]))
                      (fn [state value internal-state]
                        (Thread/sleep 1000)
                        (log-message "Read so far: " internal-state)
                        (conj internal-state value))
                      []
                      (fn [x] (log-message "Destructor called on " x)))]
      [state
       (fn [] (detach-process state process-id))]))
  
  (do (def state-and-stopper (queue-reader [1 2 3 4 :a :b :c]))
      (def state (first state-and-stopper)))

  (a-swap state (fn [x] (with-field x data [1 2 3 4 5 9])))
  (a-swap state (fn [x] (detach-all state)))

  
  (do
    (def state (atom {:numbers []}))
    (def h (loop-while-attached
            state
            (a-swap state
                   (fn [x]
                     (update-in x [:numbers]
                                (fn [r]
                                  (conj r (count r))))))
            (Thread/sleep 1000)))
    )

  (do
    (def state (atom {:a 0 :b 0 :bsum 0}))

    (def h (start-monitor
            state
            (fn [a b] (even? (:b b)))
            (fn [old-state state]
              (if (< 10 (:bsum state))
                (do
                  (log-message "Detach.")
                  (detach-from state))
                (merge state
                       {:b 0
                        :bsum (+ (:b state) (:bsum state))})))))

    (a-swap state (fn [x] (assoc x :b 2)))
                           


    )




  )


  


    
  
