(ns mummi.sparse.solve)

;; Solve Min |AX|^2, where some values in X are given.


(defn is-linear-comb? [x]
  (map? x))

(defn are-linear-combs? [x]
  (every? is-linear-comb? x))

(defn get-linear-comb-keys [x]
  (reduce
   clojure.set/union
   (map
    (fn [x]
      (set (keys x))) x)))

(defn get-all-keys [linear-combs input-set given-map]
  (clojure.set/union
   (get-linear-comb-keys linear-combs)
   input-set
   (set (keys given-map))))

;; (defn initialize-state [linear-combs input-set given-map]
;;   (let [all-keys (get-all-keys linear-combs input-set given-map)]
;;     {:all-keys all-keys
;;      :resolved (merge
;;                 (zipmap input-set nil)
;;                 given-map)
;;      :edge-set #{}
;;      :inner-set #{}
;;      :cost nil
     
              
                          
                

(defn solve [linear-combs ;; A collection of linear combs that are the rows of A.
             input-set    ;; A set of all symbols that are inputs.
             given-map]   ;; A map that maps some symbols to values.
  (assert (are-linear-combs? linear-combs))
  (assert (set? input-set))
  (assert (map? given-map)))
  
  
  
