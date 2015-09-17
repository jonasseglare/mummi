(ns mummi.obflang
  (:require [clojure.walk :refer [prewalk]]))

(defn make-generator []
  (let [counter (atom 0)]
    (fn [n]
      (let [from (deref counter)
            to (+ from n)]
        (reset! counter to)
        [from to]))))

(defn make-constructor []
  (let [gen (make-generator)]
    (fn [type ; Type can be a keyword (built-ins of this language, or a clojure function)

         result-size & args]
      (concat
       [:variable type (gen result-size)]
       args))))

(defn make-constant [value]
  [:constant value])

(defn constant? [x]
  (= :constant (first x)))

(defn variable? [x]
  (= :variable (first x)))

(defn result-size [x]
  (if (variable? x)
    (let [v (nth x 2)]
      (- (second v) (first v)))
    (do (assert (constant? x))
        1)))
        

(defn is-form? [x]
  (if (seq? x)
    (if (<= 1 (count x))
      (symbol? (first x)))))

(defn is-form-of-type? [x type]
  (if (is-form? x)
    (= type (first x))))

(defn is-if? [x]
  (is-form-of-type? x 'if))

(declare compile-x)


; To have a compile time size for it
(defn wrap-var [gen x]
  (if (constant? x)
    (gen :set 1 (second x))
    (do (assert (variable? x))
        x)))

; (wrap-var (make-constructor) (make-constant 1))
; (let [g (make-constructor)] (wrap-var g (g 'set 1 119)))

(defn compile-quote [gen x]
  (let [[type data] x]
    (assert (or (variable? data) (constant? data)))
    data))

(defn compile-if [gen x]
  (assert (is-if? x))
  (let [[type cond a b] x
        cond-result (compile-x gen cond)]
    (if (constant? cond-result)
      (if (second cond-result)
        (compile-x gen a)
        (compile-x gen b))
      (let [a-result (compile-x gen a)
            b-result (compile-x gen b)
            n (do
                (assert (= (result-size a-result)
                           (result-size b-result)))
                (result-size a-result))]
        (assert (= 1 (result-size cond-result)))
        (gen :if
             n
             cond-result
             a-result
             b-result)))))


(defn try-eval-cst [fun args]
  (if (every? (fn [x] (= (first x) :constant)) args)
    (make-constant
     (apply fun (map second args)))))
        
(defn replace-vars [varmap expr]
  (prewalk
   (fn [x]
     (let [result (if (symbol? x)
                    (if (contains? varmap x)
                      (list 'quote (get varmap x))))]
       (if result result x)))
   expr))

; (replace-vars {'a 119} '(if a 13))

(defn compile-varmap [gen x]
  (let [symbols (take-nth 2 x)
        expressions (map (fn [r] (compile-x gen r)) (take-nth 2 (rest x)))]
    (zipmap symbols expressions)))

(defn compile-let [gen x]
  (assert (is-form-of-type? x 'let))
  (let [[letsym unc-varmap unc-expr] x
        varmap (compile-varmap gen unc-varmap)]
    (compile-x gen
               (replace-vars varmap unc-expr))))

(defn make-obfuscated [sym]
  (assert (symbol? sym))
  (symbol (str "obfuscated-" (name sym))))

; (make-obfuscated 'rulle)

; To define precompiled functions that will become obfuscated.
; The more code we can obfuscate, the better
(defmacro defobf [name arglist body]
  (let [cstsym (gensym)
        argsym (gensym)
        varmapsym (gensym)
        temp (gensym)]
    `(defn ~(make-obfuscated name) [~cstsym ~argsym]
       (let [~varmapsym (zipmap (quote ~(seq arglist))
                                ~argsym)]
         (let [~temp (compile-x ~cstsym
                                (replace-vars ~varmapsym
                                              (quote ~body)))]
           ~temp)))))

(defn call-ordinary-function [fun gen compiled-args]
  (let [quick-result (try-eval-cst fun compiled-args)]
    (if quick-result quick-result
        (gen fun 1 compiled-args))))

(defn compile-x
  ([x] (compile-x (make-constructor) x))
  ([gen x]
   (assert (fn? gen))
   (cond
    (is-form? x) (let [funsym (first x)
                       fun (get {'if compile-if
                                 'quote compile-quote
                                 'let compile-let
                                 
                                 }
                                funsym)]
                   (if fun (fun gen x)
                       (let [compiled-args (map compile-x (rest x))
                             obfname (make-obfuscated funsym)]
                         (if (resolve obfname)
                           (let [custom-fun (eval obfname)]
                             (assert (fn? custom-fun))
                             (apply custom-fun [gen compiled-args]))
                           (call-ordinary-function
                            (let [fun (eval funsym)]
                              (assert (fn? fun))
                              fun)
                            gen compiled-args)))))
                           
                       

    :default (make-constant x))))
; (compile-x  1)
; (compile-x  '(if false 1 2))
; (compile-x  '(if (if true false true) 3 4))
; (compile-x  '(if (quote [:constant false]) 3 9))
; (compile-x  '(let [x false a 3 b 4] (if x a b)))
; (compile-x  '(+ 1 2 3))

;;;; TOGETHER
(defobf myif [a b c]
  (if a b c))
; (compile-x (make-generator) '(myif false 1 2))

(defobf myfak [n]
  (if (= 0 n) 1
      (* n (myfak (- n 1)))))
; (compile-x (make-constructor) '(myfak 3))


