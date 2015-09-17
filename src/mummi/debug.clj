(ns mummi.debug
  (:require [mummi.log :refer [log-message]])
  (:require [clojure.pprint :refer [pprint]])
  (:require [clojure.stacktrace :refer [print-stack-trace]]))

; Display the start and the end of the expression, what the expression is, and its value.
(defmacro scopedisp [expr]
  (let [result (gensym)
        e (gensym)]
    `(do
       (log-message (str "Begin: " ~(str expr)))
       (let [~result (try [:success ~expr]
                          (catch Throwable ~e
                            (print-stack-trace ~e)
                            [:failure (str ~e)]))]
         (if (= :success (first ~result))
           (do
             (log-message (str "End:   " ~(str expr) " Evaluates to " (second ~result)))
             (second ~result))
           (do
             (log-message (str "FAILED: " ~(str expr)))
             (throw (second ~result))))))))

(defn- make-eval [dst x]
  (let [expr-str (str x)
        var-str (str dst)
        value-sym (gensym)
        e-sym (gensym)]
    `(do (try
             (let [~value-sym ~x]
               (log-message (str ~var-str " = " ~expr-str  " = "~value-sym))
               ~value-sym)
             (catch Exception ~e-sym
               (do
                 (log-message (str "FAILED TO EVALUATE " ~var-str " = " ~expr-str ": " ~e-sym))
                 ~e-sym))))))


(defn get-bindings [args]
  (first args))

(defn get-remaining [args]
  (drop 1 args))
; (get-remaining [println [1 2 3] 4 5])
; (get-remaining [[1 2 3] 4 5])

(defn make-binding-vector [all-bindings]
  (assert (= 0 (mod (count all-bindings) 2)))
  (vec
   (loop [bindings all-bindings
          result []]
     (if (empty? bindings)
       result
       (let [var-name (first bindings)
             expr (second bindings)]
         (recur (drop 2 bindings)
                (conj result
                      var-name
                      (make-eval var-name expr))))))))
              

; A let statement where everything is displayed
(defmacro let-disp [& args]
  `(let ~(make-binding-vector (get-bindings args))
     ~@(get-remaining args)))

;; ; Evaluates expressions and returns a sequence
;; ; of values. Every expression is also displayed.
;; (defmacro disp-values [params & data-to-display]
;;   (assert (vector? params))
;;   (let [display-fun (get-log-fun params)]
;;     `[~@(map (fn [x] (make-eval x display-fun))
;;              data-to-display)]))
       
       
                               
  

(defmacro cond-disp [condition expr]
  (let [sym (gensym)
        s (str expr)]
    `(do
       (if ~condition (log-message "Evaluate expression " ~s))
       (let [~sym ~expr]
         (if ~condition (log-message "Done."))
         ~sym))))

(defmacro value-of [expr & remaining]
  (let [result (gensym)]
    `(let [~result ~expr]
       (log-message (str (quote ~expr) " = \n" (with-out-str (pprint ~result))))
       ~(if (empty? remaining)
          result
          `(value-of ~@remaining)))))

(defmacro report-errors [& statements]
  (let [e (gensym)]
    `(try
       (do ~@ statements)
       (catch Throwable ~e
         (log-message "CAUGHT EXCEPTION:")
         (log-message
          (with-out-str
            (print-stack-trace ~e)))))))

(defn defn-with-error [x]
  (if (<= 3 (count x))
    (if (vector? (nth x 2))
      (let [vx (vec x)]

        (seq
         (conj
          (subvec vx 0 3)
          `(report-errors
            ~@(seq (subvec vx 3))))))
      x)
    x))

(defn is-defn? [x]
  (if (seq? x)
    (if (<= 2 (count x))
      (= 'defn (first x)))))

(defmacro with-report-errors [& exprs]
  (let [x (gensym)]
    `(do
       ~@(map
          (fn [x]
            (if (is-defn? x)
              (defn-with-error x)
              x))
          exprs))))
