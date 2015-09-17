(ns mummi.propindexer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Basic types

(defprotocol PropNode
  "A protocol for PropIndexer nodes"

;;;;;;;;;;;;;;;;;; GET THE SUM
  (get-sum [this])
  (get-count [this])
  (is-leaf? [this]))

  

(defrecord PropInner [count sum left right]
  PropNode
  (get-sum [this] (:sum this))
  (get-count [this] (:count this))
  (is-leaf? [this] false))


(defrecord PropLeaf [obj sum]
  PropNode
  (get-sum [this] (:sum this))
  (get-count [this] 1)
  (is-leaf? [this] true))

(defn PropNode? [x]
  (or (nil? x)
      (instance? PropInner x)
      (instance? PropLeaf x)))

(defn make-prop-node [] nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-inner [a b]
  (cond
   (nil? a) b
   (nil? b) a
   :default (PropInner.
             (+ (get-count a) (get-count b))
             (+ (get-sum a) (get-sum b))
             a
             b)))



;;;;; MAIN: Insert an object with a value
(defn insert-obj [node obj value]
  (cond
   (nil? node) (PropLeaf. obj value)
   (is-leaf? node) (make-inner
                    node
                    (PropLeaf. obj value))
   :default (let [uns [(:left node) (:right node)]
                  [a b] (if (< (get-count (:left node))
                               (get-count (:right node)))
                          uns
                          (reverse uns))]
              (make-inner (insert-obj a obj value) b))))

(defn valid-index [node index]
  (let [s (get-sum node)]
    (and (<= 0 index)
         (<= index s))))

;;;;;;;;;;;;;;;;;;;;;;;;; REMOVE A NODE
(defn remove-obj [node index]
  (assert (PropNode? node))
  (assert (valid-index node index))
  (cond
   (is-leaf? node) nil
   :default
   (let [left-sum (get-sum (:left node))]
     (if (< index left-sum)
       (make-inner (remove-obj (:left node) index)
                   (:right node))
       (make-inner (:left node)
                   (remove-obj (:right node)
                               (- index left-sum)))))))

;;;;;;;;;;;;;;; GET A NODE
(defn get-node [node index]
  (assert (PropNode? node))
  (assert (valid-index node index))
  (cond
   (is-leaf? node) node
   :default
   (let [left-sum (get-sum (:left node))]
     (if (< index left-sum)
       (get-node (:left node) index)
       (get-node (:right node) (- index left-sum))))))

(defn get-leaves [init-node]
  (loop [leaves []
         node init-node]
    (if (empty? node)
      leaves
      (recur
       (conj leaves (get-node node 0))
       (remove-obj node 0)))))

(defn get-pairs [node]
  (map (fn [x]
         [(:obj x) (:sum x)])
       (get-leaves node)))


;;;;;;;;;;;; VERY USEFUL
(defn build-sub [all-pairs]
  (loop [result nil
         pairs all-pairs]
    (if (empty? pairs) result
        (recur
         (let [[obj sum] (first pairs)]
           (insert-obj result obj sum))
         (rest pairs)))))

(defn build [pairs-or-weight-map]
  (build-sub  
   (cond
    (vector? pairs-or-weight-map)  pairs-or-weight-map
    (map? pairs-or-weight-map) (do
                                 (assert (every? number? (vals pairs-or-weight-map)))
                                 (vec pairs-or-weight-map)))))
         

(defn set-node [node index new-node]
  (assert (valid-index node index))
  (assert (PropNode? node))
  (assert (PropNode? new-node))
  (cond
   (is-leaf? node) new-node
   :default
   (let [left-sum (get-sum (:left node))]
     (if (< index left-sum)
       (make-inner
        (set-node (:left node) index new-node)
        (:right node))
       (make-inner
        (:left node)
        (set-node (:right node) (- index left-sum) new-node))))))

(defn sample-all [tree0]
  (assert (PropNode? tree0))
  (loop [result []
         tree tree0]
    (if (nil? tree)
      result
      (let [index (rand (get-sum tree))]
        (recur (conj result (:obj (get-node tree index)))
               (remove-obj tree index))))))

  
  
  
;(get-node (insert-obj nil :jonas 28) 0.1) 

; (def testobjs (insert-obj (insert-obj (insert-obj (insert-obj nil :jonas 28) :johannes 28) :jovana 24) :andreas 31))
; (get-node testobjs 110)
; (get-node testobjs 30)
; (set-node testobjs 0 (PropLeaf. :gosan 26))
; (remove-obj (remove-obj (remove-obj testobjs 0) 0) 0)
; (build (get-pairs (insert-obj (insert-obj (insert-obj (insert-obj nil :jonas 28) :johannes 28) :jovana 24) :andreas 31)))


                  
             
                    


  
    
    
