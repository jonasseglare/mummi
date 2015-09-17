(ns mummi.math.lincomb
  (:require [mummi.debug :refer [value-of]])
  (:require [mummi.common :as common]))

(declare lincomb?)
(declare to-normal-form)

(defn get-args [x]
  (if (vector? x)
    (let [args (rest x)]
      (if (every? lincomb? args)
        args))))

(defn sum? [x]
  (if (vector? x)
    (and (= + (first x))
         (get-args x))))

(defn product? [x]
  (if (vector? x)
    (if (= * (first x))
      (if-let [args (get-args x)]
        (and (<= (- (count args) 1)
                 (count (filter number? args))))))))

(defn minus-signed? [x]
  (if (vector? x)
    (let [f (first x)]
      (or (= - f) (= = f)))))

(defn difference? [x]
  (if (minus-signed? x)
    (when-let [args (get-args x)]
      (let [n (count args)]
        (assert (or (= 1 n) (= 2 n)))
        (= 2 n)))))

(defn negation? [x]
  (if (vector? x)
    (if (= - (first x))
      (if-let [args (get-args x)]
        (= 1 (count args))))))

(defn scaled? [x]
  (if (vector? x)
    (if (= 2 (count x))
      (let [[num unit] x]
        (number? num)))))

(defn make-scaled [product]
  (assert (product? product))
  (let [args (get-args product)
        num (reduce * (filter number? args))
        unit (first (filter (fn [x] (not (number? x))) args))]
    [num (if (nil? unit) 1 unit)]))


(defn normal-form? [x]
  (if (and (map? x) (not (record? x)))
    (every? number? (vals x))))


(defn get-weight [normal-form key]
  (if-let [v (get normal-form key)]
    v 0))

(defn add-normal-forms [a b]
  (assert (normal-form? a))
  (assert (normal-form? b))
  (let [all-keys (clojure.set/union (set (keys a)) (set (keys b)))]
    (zipmap
     all-keys
     (map
      (fn [k]
        (+ (get-weight a k)
           (get-weight b k)))
      all-keys))))

(defn scaled-to-normal-form [x]
  (assert (scaled? x))
  (let [s (first x)
        nf (to-normal-form (second x))]
    (zipmap
     (keys nf)
     (map (fn [x] (* s x)) (vals nf)))))


(defn wrap-in-normal-form [x]
  (if (number? x)
    {1 x}
    {x 1}))


(defn negate-normal-form [x]
  (let [y (to-normal-form x)]
    (zipmap
     (keys y)
     (map - (vals y)))))


(defn negation-to-normal-form [x]
  (assert (negation? x))
  (negate-normal-form (second x)))

(defn sum-to-normal-form [x]
  (reduce
   add-normal-forms
   (map to-normal-form (get-args x))))

(defn difference-to-normal-form [x]
  (let [args (get-args x)
        [a b] (map to-normal-form args)]
    (assert (= 2 (count args)))
    (add-normal-forms a (negate-normal-form b))))

(defn product-to-normal-form [x]
  (scaled-to-normal-form
   (make-scaled x)))

(defn to-normal-form [x]
  (cond
    (normal-form? x) x
    (sum? x) (sum-to-normal-form x)
    (product? x) (product-to-normal-form x)
    (difference? x) (difference-to-normal-form x)
    (scaled? x) (scaled-to-normal-form x)
    (negation? x) (negation-to-normal-form x)
    :default (wrap-in-normal-form x)))
         
(defn lincomb? [x]
  (if (vector? x)
    (or (scaled? x)
        (sum? x)
        (product? x)
        (difference? x)
        (negation? x))
    true))

(defn make-vector
  ([params lincomb scale]
   (assert (vector? params))
   (vec
    (let [f (to-normal-form lincomb)]
      (map
       (fn [p]
         (* scale
            (if-let [v (get f p)]
              v 0)))
       params))))
  ([params lincomb]
   (make-vector params lincomb 1)))



;;;;;;;;;;;;;;;; Utilities


(defn get-normal-form-symbols [combs]
  (reduce
   clojure.set/union
   (map
    (fn [x]
      (set (keys x)))
    combs)))

(defn vmap [f a]
  (vec (map f a)))

(defn get-value-or-0 [m k]
  (if-let [v (get m k)]
    v 0))

(defn replace-symbol [normal-form old-sym new-sym]
  (if (or (contains? normal-form old-sym)
          (contains? normal-form new-sym))
    (assoc (dissoc normal-form old-sym) new-sym
             (+ (get-value-or-0 normal-form old-sym)
              (get-value-or-0 normal-form new-sym)))
    normal-form))

(defn old-new-pair? [x]
  (if (vector? x)
    (= 2 (count x))))

(defn replace-symbols [normal-form old-new-pairs]
  (assert (every? old-new-pair? old-new-pairs))
  (common/combine-repeated
   (fn [nf pair]
     (let [[old-sym new-sym] pair]
       (replace-symbol nf old-sym new-sym)))
   normal-form
   old-new-pairs))
   

(defn build-system [inputs0 costs0 constraints0]
  (let [costs (map to-normal-form costs0)
        constraints (map to-normal-form constraints0)
        symbols (clojure.set/union
                  (get-normal-form-symbols costs)
                  (get-normal-form-symbols constraints))
        inputs (vec inputs0)
        outputs (vec (clojure.set/difference symbols (set inputs)))]
     {:inputs inputs
      :outputs outputs
      :A (vmap (fn [x] (make-vector outputs x)) costs)
      :B (vmap (fn [x] (make-vector inputs x -1)) costs)
      :Ac (vmap (fn [x] (make-vector outputs x)) constraints)
      :Bc (vmap (fn [x] (make-vector inputs x -1)) constraints)}))
