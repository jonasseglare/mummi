(ns mummi.hiccuputil
  (:require [mummi.debug :refer [value-of]])
  (:require [mummi.treemap :refer :all])
  (:require [mummi.common :refer [filter-recursive simple-string-eq]]))

(defn is-hiccup-node? [x]
  (if (vector? x)
    (keyword? (first x))))

(defn get-hiccup-args [x]
  (first (filter map? x)))

(defn get-hiccup-arg [x key]
  (get (get-hiccup-args x) key))

(defn get-hiccup-arg-or-nil [x key]
  (if (is-hiccup-node? x)
    (get-hiccup-arg x key)))

(defn get-hiccup-tag [x]
  (first x))

(defn get-hiccup-strings [x]
  (filter string? x))

(defn get-hiccup-contents [x]
  (assert (is-hiccup-node? x))
  (filter (fn [y] (not (map? y)))
          (rest x)))

(defn get-hiccup-string [x]
  (first (get-hiccup-strings x)))

(defn get-hiccup-attribute [x attrib]
  (get (get-hiccup-args x) attrib))

(defn get-contained-strings [x]
  (flatten
   (map get-hiccup-strings
        (filter-recursive
         is-hiccup-node?
         x))))

(defn make-map-stripper [keys-to-keep0]
  (let [ktk (set keys-to-keep0)]
    (fn [m]
      (let [k (clojure.set/intersection
               ktk
               (set (keys m)))
            v (map (fn [x] (get m x)) k)]
        (zipmap k v)))))

(defn make-hiccup-node
  ([tag args contents]
   (if (empty? args)
     (make-hiccup-node tag contents)
     (vec (concat [tag args] contents))))
  ([tag contents]
   (vec (concat [tag] contents))))

(defn clean-hiccup-args-sub [x stripper]
  (if (is-hiccup-node? x)
    (let [tag (get-hiccup-tag x)
          args (stripper (get-hiccup-args x))
          contents (get-hiccup-contents x)]
      (make-hiccup-node
       tag args
       (map (fn [y] (clean-hiccup-args-sub y stripper))
            contents)))
    x))

(defn clean-hiccup-args
  ([x] (clean-hiccup-args x []))
  ([x keys-to-keep]
   (clean-hiccup-args-sub x (make-map-stripper keys-to-keep))))

(defn empty-hiccup-node? [x]
  (if (is-hiccup-node? x)
    (every? (fn [x] x) (map empty-hiccup-node? (get-hiccup-contents x)))))

(defn remove-empty-hiccup-nodes [x]
  (if (is-hiccup-node? x)
    (let [tag (get-hiccup-tag x)
          args (get-hiccup-args x)
          contents (map
                    remove-empty-hiccup-nodes
                    (filter
                     (fn [y] (not (empty-hiccup-node? y)))
                     (get-hiccup-contents x)))]
      (make-hiccup-node tag args contents))
    x))

(defn remove-tags-and-simplify
  ([x] (remove-tags-and-simplify x #{}))
  ([x exclude]
   (assert (set? exclude))
   (let [f (fn [y] (remove-tags-and-simplify y exclude))]
     (if (is-hiccup-node? x)
       (let [contents (get-hiccup-contents x)]
         (if (contains? exclude (get-hiccup-tag x))
           (vec
            (concat
             [(get-hiccup-tag x) (get-hiccup-args x)]
             (map f contents)))
           (if (= 1 (count contents))
             (f (first contents))
             (vec (map f contents)))))
         x))))

(defn full-hiccup-simplification [x]
  (remove-tags-and-simplify (remove-empty-hiccup-nodes x)))

     
(defn lookup-tree-key [x key]
  (cond
   (or (seq? x) (vector? x))
   (if (not (empty? x))
      (if (= (first x) key)
        (rest x)
        (loop [args x]
          (if (empty? args) nil
              (let [result (lookup-tree-key (first args) key)]
                (if result result
                    (recur (rest args))))))))
   (map? x) (lookup-tree-key (vals x) key)
   :default nil))

(defn simplify-but-links-and-ids [x]
  (clean-hiccup-args
   (remove-empty-hiccup-nodes x)
   [:href :id]))

(defn is-hiccup-node-of-type? [x type]
  (if (is-hiccup-node? x)
    (= type (get-hiccup-tag x))))

(defn is-hiccup-link? [x]
  (is-hiccup-node-of-type? x :a))

(defn flatten-hiccup-contents [x]
  (if (is-hiccup-node? x)
    (flatten (map flatten-hiccup-contents (get-hiccup-contents x)))
    x))


(defn get-links-with-text
  ([x txt] (get-links-with-text x txt simple-string-eq))
  ([x txt cmp]
   (filter-recursive
    (fn [y]
      (if (is-hiccup-link? y)
        (cmp txt (apply str (flatten-hiccup-contents y)))))
    x)))

(defn get-nodes-with-id-like [x pattern]
  (filter-recursive
   (fn [y]
     (if (is-hiccup-node? y)
       (let [s (get-hiccup-arg y :id)]
         (if (string? s)
           (re-matches pattern s)))))
   x))

(defn get-hiccup-links [x]
  (filter-recursive
   is-hiccup-link?
   x))

(defn make-hiccup-map [x]
  (make-tree-map
   x
   (fn [y]
     (if (is-hiccup-node? y)
       (get-hiccup-contents y)))))

; For the hiccup-map
(defn filter-inds-by-tag [m tag]
  (filter-inds
   (fn [x] (is-hiccup-node-of-type? x tag))
   m))

(defn make-value-fn [x]
  (cond
   (fn? x) x
   
   (= (class #"asdf") java.util.regex.Pattern)
   (fn [y] (re-matches x y))
   :default (fn [y] (= y x))))

(defn filter-inds-by-arg [m key value-fn]
  (if (not (fn? value-fn))
    (filter-inds-by-arg m key (make-value-fn value-fn))
    (filter-inds
     (fn [x]
       (if (is-hiccup-node? x)
         (value-fn (get-hiccup-arg-or-nil x key))))
     m)))

(defn filter-inds-by-string [m value-fn]
  (if (not (fn? value-fn))
    (filter-inds-by-string m (make-value-fn value-fn))
    (filter-inds
     (fn [x]
       (if (string? x)
         (value-fn x)))
     m)))
       

;(defn has-any-hiccup-arg [args x]
  
