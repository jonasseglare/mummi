(ns mummi.math.cstlevmar
  (:require [mummi.math.levmar :as levmar])
  (:refer-clojure :exclude [* - + == /])
  (:require [mummi.math.utils :refer [mat-to-vec vec-to-col]])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all])
  (:require [clojure.core.matrix.linear :refer :all])
  (:require [mummi.math.ops :refer [solve-with-cst]]))



; min-state and cst-state only need to include :objf.
;
; They can optionally include :objf-j for analytic Jacobian
;
; Optionally, they can also include various settings.
(defn take-cst-step [min-state cst-state X]
  (assert (= 1 (dimensionality X)))
  (let [unc (levmar/take-levmar-step (assoc min-state :X X))
        [Fc Jc] ((:objf-j cst-state) X)
        projX (solve-with-cst (identity-matrix (count X))
                              (:X unc)
                              Jc
                              (- (mmul Jc X) Fc))
        new-cst-state (levmar/take-levmar-step (assoc cst-state :X projX))]
    [unc new-cst-state (:X new-cst-state)]))
    
    

(defn run-cst-levmar [min-problem cst-problem initX]
  (assert (nil? (:initX min-problem)))
  (assert (nil? (:initX cst-problem)))
  (loop [min-state (levmar/make-full-state (assoc min-problem :initX initX))
         cst-state (levmar/make-full-state (assoc cst-problem :initX initX))
         X initX]
    (if (or (:stop min-state)
            (:stop cst-state))
      [min-state cst-state]
      (let [[next-min next-cst X] (take-cst-step min-state cst-state X)]
        (recur next-min next-cst X)))))

(defn cst-demo []
  (run-cst-levmar
   {:objf (fn [X] [(- (mget X 0) 9)])}
   {:objf (fn [X] [(- (norm X) 1)])}
   [3 -4]))
