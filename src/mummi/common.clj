(ns mummi.common
  (:require [clojure.pprint :as pprint]))

(defn try-parse-int [x]
  (try
    (Integer. x)
    (catch Throwable e nil)))

(defn try-parse-double [x]
  (try
    (Double. x)
    (catch Throwable e nil)))

(defn make-record [x]
  (assert (record? x))
  [(.getName (class x)) (merge {} x)])

(defn vecseq?
  ([x] (or (vector? x) (seq? x)))
  ([x n] (and (= n (count x))
              (vecseq? x))))

(defn on-nil [x value]
  (if (nil? x) value x))

(defn make-symbol [& args]
  (symbol
   (apply str (map name args))))

(defn get-pair-inds [index size]
  [(mod index size) (int (/ index size))])

(defn map-vals-recur [m fun]
  (let [recfun (fn [x]
                 (if (and (map? x) (not (record? x)))
                   (map-vals-recur x fun)
                   (fun x)))
        k (keys m)
        v (map recfun (vals m))]
    (zipmap k v)))

(defn subind [A B]
  (map (fn [index]
         (nth A index))
       B))

(defn logical-slice [A B]
  (assert (= (count A) (count B)))
  (let [n (count A)]
    (subind A (filter (fn [index] (nth B index)) (range n)))))

(defn make-counter []
  (let [counter (atom 0)]
    (fn []
      (let [value (deref counter)]
        (swap! counter (fn [x] (+ 1 x)))
        value))))

(defmacro do-first [& args]
  (let [x (gensym)]
    `(let [~x ~(first args)]
       ~@args
       ~x)))

(defn map-with-keys? [x & keys]
  (if (map? x)
    (every? (fn [key] (contains? x key)) keys)))

; (map-vals-recur {:a {:b 3, :c 4}, :d 10} (fn [x] (* x x)))
(defn list-values [m keys]
  (map
   (fn [key]
     (get m key))
   keys))

(defn make-record-from-map [record-from-vec-fun m keys]
  (apply record-from-vec-fun
         (list-values m keys)))

(defn underscores-to-dashes [x]
  (.replace x "_" "-"))

(defn resolve-record-data [full-record-name0]
  (let [full-record-name (underscores-to-dashes full-record-name0)
        index (.lastIndexOf full-record-name ".")
        prefix (subs full-record-name 0 index)
        suffix (subs full-record-name (+ 1 index))
        make-from-map-name (str "make-" suffix "-from-map")]
    {:make-from-map-name make-from-map-name
     :full-make-from-map-name (str prefix "/" make-from-map-name)
     :ns prefix}))


      

(defmacro defrecord2 [record-name fields]
  (assert (symbol? record-name))
  (assert (vector? fields))
  (assert (every? symbol? fields))
  (let [make-from-map-name (make-symbol "make-" record-name "-from-map")
        make-from-map-name2 (make-symbol "map->" record-name)
        make-from-vec-name (make-symbol "->" record-name)
        make-from-args-name (make-symbol "make-" record-name "-from-args")
        keyword-fields (vec (map keyword fields))
        args (gensym)
        x (gensym)]
    `(let [rname# (defrecord ~record-name ~fields)]

       (defn ~make-from-map-name [m#]
         (~make-from-map-name2 m#))

       (def ~(make-symbol record-name "-deserializer")
         {:record-name rname#
          :deserializer
          (fn [x#]
            (make-record-from-map
             ~make-from-vec-name
             x# ~keyword-fields))})
                                  

       (defn ~make-from-args-name [& ~args]
         (let [dif# (- ~(count fields) (count ~args))]
         (assert (<= 0 dif#))
         (apply ~make-from-vec-name (concat ~args (take dif# (repeat nil))))))
         

       (defn ~(symbol (str (name record-name) "?")) [~x]
         (map-with-keys? ~x ~@(map keyword fields))))))

(defmacro wrap-errors [& exprs]
  (let [x (gensym)]
    `(try
       [:success (do ~@exprs)]
       (catch Throwable ~x
         [:exception ~x]))))

(defmacro check [pred-fun value]
  `(do
     (assert (~pred-fun ~value))
     ~value))

(defn rotate [x steps]
  (let [vx (vec x)
        len (count x)]
    (map (fn [index]
           (nth vx (mod (+ index steps) len)))
         (range len))))

(defn pair-with-indices [X]
  (map (fn [a b] [a b])
       X
       (range (count X))))

(defn select-pair-by [f a b]
  (if (f (first a)
         (first b))
    a
    b))

(defn extreme-val-and-index [f X]
  (reduce (fn [a b]
            (select-pair-by f a b))
          (pair-with-indices X)))

(defn sort-vals-and-inds [f X]
  (sort (fn [a b]
          (f (first a)
             (first b)))
        (pair-with-indices X)))

(defn min-inds [X]
  (map second (sort-vals-and-inds < X)))

(defn max-inds [X]
  (map second (sort-vals-and-inds > X)))
  

(defn min-val-and-index [X]
  (extreme-val-and-index < X))

(defn max-val-and-index [X]
  (extreme-val-and-index > X))

(defn ind2sub [size0 index0]
  (loop [size size0
         inds []
         index index0]
    (if (empty? size)
      inds
      (recur (rest size)
             (conj inds (mod index (first size)))
             (int (/ index (first size)))))))

(defn check-finite [x]
  (assert (Double/isFinite x))
  x)

(defn sub2ind [size0 inds0]
  (assert (= (count size0) (count inds0)))
  (loop [index 0
         size (reverse size0)
         inds (reverse inds0)]
    (if (empty? size)
      index
      (recur (+ (* index (first size))
                (first inds))
             (rest size)
             (rest inds)))))

(declare make-parent-map)

(defn fill-map [parent m-added data]
  (loop [elements data
         m m-added]
    (if (empty? elements)
      m
      (recur
       (rest elements)
       (make-parent-map (first elements) m data)))))

(defn add-parent [m key parent]
  (let [current (if (contains? m key)
                  (get m key)
                  [:parents])]
    (assoc m key (conj current parent))))

(defn make-parent-map
  ([data] (make-parent-map data {} nil))
  ([data init-m parent]
   (let [m-added (add-parent init-m data parent)]
     (cond
      (or (seq? data) (vector? data)) (fill-map data m-added data)
      (map? data) (fill-map
                   data
                   (fill-map data m-added (keys data))
                   (vals data))
      :default m-added))))

(defn nth-parent [parentmap key0 n0]
  (loop [n n0
         x key0]
    (if (= n 0) x
        (recur (- n 1)
               (second (get parentmap x))))))


(declare filter-recursive)

(defn filter-recursive-sub [f data0 result0]
  (loop [result result0
         data data0]
    (if (empty? data)
      result
      (recur (filter-recursive f (first data) result)
             (rest data)))))

; Traverses a datastructure and returns a vector of all nodes
; in this structure for which f returns true
(defn filter-recursive
  ([f data] (filter-recursive f data []))
  ([f data result0]
   (let [result1 (if (f data)
                   (conj result0 data)
                   result0)]  
    (cond
     (map? data) (filter-recursive-sub
                  f (keys data)
                  (filter-recursive-sub
                   f (vals data) result1))
     (coll? data) (filter-recursive-sub f data result1)
     :default result1))))
      
;SHOULD EVAL TO 99:  (sub2ind [39 434] (ind2sub [39 434] 99))
             
(defn successors-of [data0 key]
  (loop [data data0]
    (cond
     (empty? data) nil
     (= key (first data)) (rest data)
     :default (recur (rest data)))))

(defn standardize-string [x]
  (clojure.string/trim (clojure.string/upper-case x)))

(defn get-nil-keys [m]
  (filter nil? (keys m)))


; Case insensitive and trimmed.
(defn simple-string-eq [a b]
  (if (and (string? a) (string? b))
    (= (standardize-string a)
       (standardize-string b))))

(defn loop-while-change-sub [init-expr expr]
  (let [x (gensym)
        y (gensym)]
     `(loop [~x ~init-expr]
        (let [~y ~expr]
          (if (= ~x ~y)
            ~y
            (recur ~y))))))


(defmacro loop-while-change [init-expr & expr]
  (if (empty? expr)
    (loop-while-change-sub init-expr init-expr)
    (loop-while-change-sub init-expr (first expr))))


; Make a new map key
(defn make-key [m]
  (assert (map? m))
  (let [nums (filter number? (keys m))]
    (if (empty? nums) 0
        (+ 1 (reduce max nums)))))


(defmacro with-field
  [object field-name & expr]
  `(update-in
    ~object
    [~(keyword field-name)]
    (fn [~(symbol field-name)]
      ~@expr)))

(defmacro with-map-entry [object name-and-key & expr]
  (let [[var-name key] name-and-key]
    `(update-in
      ~object
      [~key]
      (fn [~var-name]
        ~@expr))))

(defmacro defprotocol2 [& args]
  (let [x (gensym)
        proto-name (first args)]
    `(do
       (defprotocol ~@ args)
       (defn ~(symbol (str (name proto-name) "?")) [~x]
         (satisfies? ~proto-name ~x)))))

(defn filter-map [pred m]
  (assert (fn? pred))
  (assert (map? m))
  (into
   {}
   (filter
    (fn [e]
      (pred e))
    (vec m))))

(defn filter-map-by-value [pred m]
  (filter-map
   (fn [e]
     (pred (second e)))
   m))

(defn filter-map-by-key [pred m]
  (filter-map
   (fn [e]
     (pred (first e)))
   m))

(defn map-map [fun m]
  (assert (fn? fun))
  (assert (map? m))
  (into
   {}
   (map
    fun m)))

(defn map-vals [fun m]
  (map-map
   (fn [x]
     [(first x) (fun (second x))])
   m))

(defn map-keys [fun m]
  (map-map
   (fn [x]
     (let [[a b] x]
       [(fun a) b]))
   m))

(defn with-lock-fun [lock fun]
  (let [p (promise)]
    (swap! lock
           (fn [x] (if x
                     (do (deliver p false)
                         x)
                     (do (deliver p true)
                         true))))
    (when (deref p)
      (fun)
      (reset! lock false))))

; The body will only be executed if the lock can be locked.
(defmacro with-lock [lock & body]
  `(with-lock-fun ~lock (fn [] ~@body)))
  
(defn clamp
  ([x a b]
   (if (< x a) a
       (if (< b x) b
           x)))
  ([x interval]
   (let [[a b] interval]
     (clamp x a b))))

(defn combine-repeated [fun init coll]
  (loop [remaining coll
         result init]
    (if (empty? remaining)
      result
      (recur (rest remaining)
             (fun result (first remaining))))))

(defn near [A B tol]
  (let [lnear (fn [a b] (near a b tol))]
    (if (vecseq? A)
      (every? identity (map lnear A B))
      (do (assert (number? A))
          (assert (number? B))
          (< (Math/abs (- A B)) tol)))))

(defn in-range? [index start end]
  (and (<= start index) (< index end)))

(defn nthnil [coll index]
  (if (vecseq? coll)
    (if (in-range? index 0 (count coll))
      (nth coll index))))
      
(defn disp-class-paths []
  (println (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader)))))

(defn boolean? [x]
  (instance? Boolean x))

(defmacro def-reduced [fname arglist & body]
  (assert (vector? arglist))
  (assert (= 2 (count arglist)))
  (assert (symbol? fname))
  `(defn ~fname [& args#]
     (reduce
       (fn ~arglist
         ~@body)
       args#)))

(defmacro validate-data [validator data]
  (let [x `(~validator ~data)]
    `(let [results# ~data]
       (if (~validator results#) results#
         (throw
          (RuntimeException.
           (str
            ~(str "Invalid data: The call "
                  (with-out-str
                    (pprint/pprint x))
                  " evaluates to false, with " data )
            (with-out-str
              (pprint/pprint ~data)))))))))

(defn replace-symbols-in-body [src dst body]
  (clojure.walk/postwalk-replace
   (zipmap src dst)
   body))

(defmacro def-template [name symbols-to-replace & body]
  (assert (symbol? name))
  (assert (vector? symbols-to-replace))
  (let [symbols (gensym)]
    `(def ~name
       {:symbols-to-replace (quote ~symbols-to-replace)
        :body (quote ~body)})))

(defn positive-integer? [x]
  (and (integer? x)
       (< 0 x)))

(defn template? [x]
  (map-with-keys? x :body :symbols-to-replace))


(defmacro instantiate [template-name dst]
  (let [template (eval template-name)]
    (assert (template? template))
    `(do ~@(replace-symbols-in-body
            (:symbols-to-replace template)
            dst
            (:body template)))))

(defn repeated-apply [f n value]
  (validate-data fn? f)
  (validate-data number? n)
  (loop [m n
         out value]
    (if (= 0 m) out
        (recur (- m 1) (f out)))))


(comment
  
  (with-template make-add-3 [my-add]
    (defn add-3 [a b c]
      (my-add a b c)))

  )
