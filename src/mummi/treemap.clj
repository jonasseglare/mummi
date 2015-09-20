(ns mummi.treemap
  (:require [mummi.common :refer [map-with-keys?]]))

(defn make-tree-map-from-single-node [x]
  [{:index 0 :data x :parent nil}])

(defn add-child-to-map-entry [dst child]
  (if (nil? (:children dst))
    (assoc dst :children (set [child]))
    (assoc dst :children
           (conj (:children dst) child))))

(defn is-tree-map? [x]
  (if (vector? x)
    (if (empty? x) true
        (map-with-keys? (first x) :index :parent :data))))

(defn insert-children [m]
  (assert (is-tree-map? m))
  (loop [v (range (count m))
         result m]
    (if (empty? v)
      result
      (recur (rest v)
             (let [index (first v)
                   parent-index (:parent (get result index))]
               (if (nil? parent-index)
                 result
                 (assoc result
                   parent-index
                   (add-child-to-map-entry (get result parent-index) index))))))))

               
  
    

; Makes a tree map from a tree node.
(defn make-tree-map-sub
  ([x get-children] (make-tree-map-sub x get-children [] nil 0 0))
  ([x get-children state0 parent depth child-index]
   (let [index (count state0)
         first-state (assoc
                         state0 index
                         {:index index :parent parent :data x :depth depth
                          :child-index child-index})
         children (get-children x)]
     (if children
       (loop [counter 0
              state first-state
              contents children]
         (if (empty? contents)
                                        ; Update the state of this node
           (assoc state index

                                        ; With the same node, but :children added to it
                  (assoc (get state index) :descendants
                         (range (+ 1 index) (count state))))

           (recur (+ 1 counter)
                  (make-tree-map-sub (first contents)
                                     get-children
                                     state index (+ depth 1) counter)
                  (rest contents))))
       first-state))))

(defn make-tree-map [x get-children]
  (insert-children
   (make-tree-map-sub x get-children)))


; Lists indices of all nodes that satisfy something,
; as a set.
(defn filter-inds [fun hmap]
  (assert (fn? fun))
  (assert (is-tree-map? hmap))
  (set
   (map
    :index
    (filter
     (fn [x]
       (fun (:data x)))
     hmap))))

(defn tree-descendants [hmap inds]
  (if (set? inds)
    (reduce
     clojure.set/union
     (map
      (fn [x] (tree-descendants hmap x))
      inds))
    (set
     (:descendants (get hmap inds)))))

(defn tree-children [hmap inds]
  (if (set? inds)
    (reduce
     clojure.set/union
     (map
      (fn [x] (:children (get hmap x)))
      inds))
    (tree-children hmap (set [inds]))))

(defn tree-parents [hmap inds]
  (if (set? inds)
    (reduce
     clojure.set/union
     (map
      (fn [x] (set [(:parent (get hmap x))]))
      inds))
    (tree-parents hmap (set [inds]))))
      

(defn tree-follows [hmap inds]
  (if (not (set? inds))
    (tree-follows hmap #{inds})
    (set
     (range
      (reduce min inds)
      (count hmap)))))
    


;(defn children
(defn tree-ascendants
  ([hmap inds] (tree-ascendants hmap inds #{}))
  ([hmap inds0 result0]
   (if (set? inds0)
     (loop [result result0
            inds inds0]
       (if (empty? inds)
         result
         (recur (clojure.set/union
                 result
                 (tree-ascendants hmap (first inds) result))
                (rest inds))))
     (do (assert (number? inds0))
         (loop [result result0
                index (:parent (get hmap inds0))]
           (if (nil? index)
             result
             (recur (conj result index)
                    (:parent (get hmap index)))))))))
  
(defn tree-deepest [m inds]
  (let [depths (map
                (fn [x]
                  (:depth (nth m x)))
                inds)
        max-depth (do
                    (assert (not (empty? depths)))
                    (reduce max depths))
        pairs (map vector depths inds)]
    (map
     second
     (filter
      (fn [x]
        (= (first x) max-depth))
      pairs))))
