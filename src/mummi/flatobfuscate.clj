(ns mummi.flatobfuscate)

(defn primitive-data? [b]
  (not (or (seq? b) (vector? b) (map? b))))

(defn make-complex-data [x]
  [:data x])

(defn make-data [x]
  (assert (primitive-data? x))
  (make-complex-data x))


(defn data? [x]
  (if (vector? x)
    (if (= 2 (count x))
      (let [[a b] x]
        (and
         (= :data a)
         (primitive-data? b))))))

(defn make-bracket [begin-or-end type]
  (assert (contains? #{:begin :end} begin-or-end))
  (assert (contains? #{:map :vector :seq} type))
  [:bracket begin-or-end type])

  

(defn bracket? [x]
  (if (vector? x)
    (if (= 3 (count x))
      (let [[a b c] x]
        (and (= :bracket a)
             (keyword? b)
             (keyword? c))))))

(defn bracket-of-type? [x type]
  (if (bracket? x)
    (= type (second x))))

(defn opening-bracket? [x]
  (bracket-of-type? x :begin))

(defn closing-bracket? [x]
  (bracket-of-type? x :end))

(defn bracket-type [br]
  (nth br 2))

(defn make-opening-bracket [closing-bracket]
  (assert (closing-bracket? closing-bracket))
  (make-bracket :begin (bracket-type closing-bracket)))

(defn leaf? [x]
  (or (data? x) (bracket? x)))



(declare flatten-expr-sub)

(defn flatten-map-sub [x]
  (assert (map? x))
  (map
   (fn [x]
     (let [[a b] x]
       [(flatten-expr-sub a)
        (flatten-expr-sub b)]))
   (vec x)))
  

(defn flatten-map [x]
  (assert (map? x))
  [(make-bracket :begin :map)
   (flatten-map-sub x)
   (make-bracket :end :map)])

(defn flatten-vector [x]
  [(make-bracket :begin :vector)
   (map flatten-expr-sub x)
   (make-bracket :end :vector)])

(defn flatten-seq [x]
  [(make-bracket :begin :seq)
   (map flatten-expr-sub x)
   (make-bracket :end :seq)])

(defn flatten-expr-sub [x]
  (cond
   (map? x) (flatten-map x)
   (vector? x) (flatten-vector x)
   (seq? x) (flatten-seq x)
   :default (make-data x)))

(defn flatten-postproc [x]
  (cond
   (empty? x) '()
   (leaf? (first x))
   (do 
       (cons (first x)
             (flatten-postproc (rest x))))
   :default (concat (flatten-postproc (first x))
                    (flatten-postproc (rest x)))))

(defn flatten-expr [x]
  (flatten-postproc
   (flatten-expr-sub x)))


(defn split-stack [stack cb]
  (let [index (.lastIndexOf stack (make-opening-bracket cb))]
    [(subvec stack 0 index)
     (subvec stack (+ 1 index))]))


(defn compose-structure [type src-data]
  (let [data (map second src-data)]
    (cond
     (= :seq type) (seq data)
     (= :vector type) (vec data)
     :default (do
                (assert (= :map type))
                (let [keys (take-nth 2 data)
                      vals (take-nth 2 (rest data))]
                  (zipmap keys vals))))))
       
       
(defn rebuild [init-x]
  (loop [elements init-x
         stack []]
    (if (empty? elements)
      (second (first stack))
      (let [e (first elements)]
        (if (closing-bracket? e)
          (let [[next-stack src-data] (split-stack stack e)]
            (recur (rest elements)
                   (conj
                    next-stack
                    (make-complex-data
                     (compose-structure (bracket-type e)
                                        src-data)))))
          (recur (rest elements)
                 (conj stack e)))))))

(def testdata '(defn my-wrap [type data]
                 (cond
                  (= type :seq) (list :data data)
                  (= type :vector) [:data data]
                  :default {:data data})))
(assert (= testdata (rebuild (flatten-expr testdata))))

; Obfuscates an expression by flattening it
; to a symbolic form that can be rebuilt
(defmacro flatten-obfuscate [expr]
  `(eval (rebuild (quote ~(flatten-expr expr)))))

; Obfuscates certain listed functions
;(defn funcall-obfuscate [exclude body]

; (basic-obfuscate (do (defn fak [x] (if (= 0 x) 1 (* x (fak (- x 1))))) (fak 10)))
