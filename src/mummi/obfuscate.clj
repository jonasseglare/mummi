(ns mummi.obfuscate
  (:require [mummi.common :refer :all])
  (:require [clojure.walk :refer :all]))

; Replace an expression by 
; a call to a function that evaluates the expression

(defn bound-to-fun?
  ([x] (bound-to-fun? x #{}))
  ([x defns]
   (assert (set? defns))
   (or (contains? defns x)
       (try
         (if (resolve x)
           (fn? (eval x)))
         (catch Exception e nil)))))

(defn is-s-expr? [x]
  (if (seq? x)
    (if (not (empty? x))
      (symbol? (first x)))))
      

(defn is-fun-call? [x defns]
  (if (is-s-expr? x)
    (bound-to-fun? (first x) defns)))

(defn get-args [x]
  (assert (is-s-expr? x))
  (rest x))

(defn get-fun [x]
  (first x))


;;;;;;;;;;;;; First technique
;
; Wrap the expression inside an anonymous 
; function that we evaluate directly.
;
; X -> ((fn [] X))
(defn funwrap-0 [x]
  `((fn [] ~x)))
; (funwrap-0 '(+ 1 3))



;;;;;;;;;;;;;; Second technique
;
; Shuffle the arguments and call the original function with them shuffled. 
;
; X -> X
; (f a b c) -> ((fn [i j] (f a i j)) b c)
;

(defn extract-seq-template [k]
  (let [r (gensym)
        res (gensym)
        n (gensym)]
  `(loop [~r ~k
          ~res []]
     (if (empty? ~r)
       ~res
       (let [~n (count ~r)]
         (recur
          (if (even? ~n) (subvec ~r 1) (subvec ~r 0 (- ~n 1)))
          (conj ~res (if (even? ~n) (first ~r) (last ~r)))))))))

(defmacro extract-seq-m [k]
  (extract-seq-template k))

(defn extract-seq [k]
  (extract-seq-m (vec k)))

         

(defn make-shuffled-maps [X]
  (if (number? X)
    (make-shuffled-maps (shuffle (range X)))
    (let [n (count X)]
      (let [X (map (fn [a b]
                     [a b])
                   X
                   (range n))
            mkcmp (fn [fun] (fn [a b] (< (fun a) (fun b))))]
        [(map first (sort (mkcmp second) X))
         (map second (sort (mkcmp first) X))]))))

(defn funwrap-2 [x]
  (let [n (count x)
        [forw backw] (make-shuffled-maps (extract-seq (range n)))
        res (gensym)]
    `(let [~res ~(extract-seq-template (vec (subind x backw)))]
       (apply (first ~res) (rest ~res)))))

(defn funwrap-1 [x]
  (let [args (get-args x)
        inds (range (count args))
        mask (shuffle (map (fn [x] (< (* 2 x) (count args))) inds))
        argsyms (map (fn[x] (gensym)) (range (count args)))
        [A B] (make-shuffled-maps (count args))
        C (map (fn [a b] (if a b nil)) mask argsyms)]
    `((fn [~@(logical-slice argsyms mask)]
        (~(get-fun x)
         ~@(let [tmp (subind C A)]
             (map (fn [index]
                    (let [x (nth tmp index)]
                      (if x x
                          (nth args index))))
                  (range (count args))))))
      ~@(logical-slice (subind args B) mask))))

; (funwrap-1 '(list 1 2 3 4 5))

(defn protect [x]
  x)

(defn protected? [x]
  (if (is-s-expr? x)
    (= 'protect (first x))))

(defn walk-pgm [fun pgm defn-syms]
  (let [walker (fn [x] (walk-pgm fun x defn-syms))]
    (cond
     (vector? pgm) (vec (map walker pgm))
     (map? pgm) (zipmap
                 (map walker (keys pgm))
                 (map walker (vals pgm)))
     (protected? pgm) pgm
     (seq? pgm) (if (is-fun-call? pgm defn-syms)
                  (fun (cons
                        (first pgm)
                        (map walker (rest pgm))))
                  (seq (map walker pgm)))
     :default pgm)))

(defn obfuscate-random-nonrecursive [expr funwraps]
  ((nth funwraps (rand (count funwraps))) expr))

(defn obfuscate-recursive [expr funwraps defn-syms]
  (walk-pgm
   (fn [x] (obfuscate-random-nonrecursive x funwraps))
   expr
   defn-syms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Defn obfuscation
;
(defn is-defn? [x]
  (if (is-s-expr? x)
    (= 'defn (first x))))


 (defn find-defn-symbs-sub [x]
   (cond
    (is-defn? x) (second x)
    (or (map? x) (vector? x) (seq? x)) (map find-defn-symbs-sub x)
    :default []))

(defn find-defn-symbs [x]
  (flatten (find-defn-symbs-sub x)))
   
  

(defn obfuscate-defns [x]
  (let [defn-syms (find-defn-symbs x)
        new-syms (map (fn [x] (gensym)) defn-syms)
        m (zipmap defn-syms new-syms)
        rp (fn [x] (if (contains? m x)
                       (get m x)
                       x))]
    [(set new-syms) (postwalk rp x)]))



(defn obfuscate-if [x]
  (if (if (is-s-expr? x)
        (= 'if (first x)))
    (let [[if-sym cond a b] x
          Rsym (gensym)
          Bsym (gensym)]
      `((fn [~Bsym]
          (let [~Rsym (reverse ~Bsym)]
            (if ((second ~Rsym))
              ((last ~Rsym))
              ((first ~Rsym)))))
        (reverse [(fn [] ~b) (fn [] ~cond) (fn [] ~a)])))
    x))

(defn obfuscate-if-recursive [x]
  (postwalk obfuscate-if x))
        
(defn full-obfuscation [x]
  (let [[defn-syms obf-defn-expr] (obfuscate-defns x)]
    (obfuscate-if-recursive
     (obfuscate-recursive obf-defn-expr [funwrap-1] defn-syms))))
        
(defmacro obfuscate [x]
  (full-obfuscation x))


; 
; (full-obfuscation (full-obfuscation (full-obfuscation '(do (defn fak [n] (if (protect (= 0 n)) 1 (* n (fak (- n 1))))) (fak 9)))))

;;;;;;;;;;;;;; Third technique
;
; Hide the data in several data structures.
;
; First make an index list (range (+ 1 (count args))) ; For the function also
;
; Then, for every index, insert it into a data structure.
; Then, compute an expression to access every argument given a symbol
;  holding the data structure.
; Then, make a function that picks out the function and calls it on the arguments.

;;;;;;;;;;;;;;; Fourth technique
;
; For all defn's 
