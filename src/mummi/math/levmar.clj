(ns mummi.math.levmar
  (:require [mummi.common :refer [check-finite map-with-keys?]])
  (:require [mummi.debug :refer [value-of scopedisp]])
  (:require [mummi.math.ops :refer :all])
  (:require [clojure.pprint :refer :all])
  (:refer-clojure :exclude [* - + == /])
  (:require [mummi.math.utils :refer [mat-to-vec vec-to-col]])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all])

                                        ; To get solve
  (:require [clojure.core.matrix.linear :refer :all]))

(def stopcodes
  {:f-close-to-0 "|F| or |JtF| is close to 0"
   :undefined "Undefined stopping reason"
   :no-solution "No solution to linear system"
   :minstep "Step size is minimum"
   :maxiter "Maximum number of iterations reached"})


(def settings
  {:maxiter 120
   :e1 1.0e-15
   :e2 1.0e-15
   :e3 1.0e-15
   :tau 1.0e-3
   :verbosity 0})

(defn max-abs-diag-element [M]
  (let [[rows cols] (shape M)]
    (assert (= rows cols))
    (reduce
     max
     (map (fn [i]
            (Math/abs (mget M i i)))
          (range rows)))))

(defn max-abs-element [X]
  (reduce
   max
   (map (fn [x] (Math/abs x)) (eseq X))))

(defn make-levmar-state [X]
  {:counter 0
   :X X
   :v 2.0
   :mu nil
   :stop false})

(defn test-objf [X]
  {:J (identity-matrix (count X))
   :F X})

(defn make-local-state [state]
  (let [objf (:objf-j state)
        settings (:settings state)
        X (:X state)
        [F J] (objf X)
        JtJ (mmul (transpose J) J)]
    (merge state
           {:rho -1
            :JtJ JtJ
            :J J
            :F F
            :JtF (mmul (transpose J) F)
            :mu
            (if (nil? (:mu state))
              (* (:tau settings) (max-abs-diag-element JtJ))
              (:mu state))
            :norm2F (square (norm F))
            :normX (norm X)})))

(defn quit-loop? [state]
  (or (:stop state)
      (> (:rho state) 0)))

(defn stop
  ([state] (stop state :undefined))
  ([state stopcode]
   (merge state {:stop true
                 :stopcode stopcode})))

(defn compute-dx [state]
  (let [[jrows jcols] (shape (:JtJ state))]
    (assert (Double/isFinite (:mu state)))
    (let [solution (solve (+ (:JtJ state)
                             (* (:mu state)
                                (identity-matrix jrows)))
                          (:JtF state))]
      (if (nil? solution)
        (stop state :no-solution)
        (assoc state :dX (- solution))))))

(defn minimum-step-size? [state]
  (< (norm (:dX state)) (* (:e2 (:settings state)) (:normX state))))


(defn attempt-step [state]
  (let [Xnew (+ (:X state) (:dX state))
        Fnew ((:objf state) Xnew)
        norm2Fnew (square (norm Fnew))]
    (merge state
           {:Xnew Xnew
            :Fnew Fnew
            :norm2Fnew norm2Fnew
            :rho (/ (- (:norm2F state) norm2Fnew)
                    (dot (:dX state) (- (* (:mu state) (:dX state))
                                        (:JtF state))))

            })))

(defn improvement? [state]
  (> (:rho state) 0))

(defn accept-state [state]
  (merge state
         {:X (:Xnew state)
          :mu
          (check-finite
           (* (:mu state)
              (max (/ 1.0 3)
                   (- 1 (Math/pow (- (* 2.0 (:rho state)) 1) 3)))))
          :v 2}))

(defn increase-damping [state]
  (merge state
         {:mu (check-finite (* (:v state) (:mu state)))
          :v (check-finite (* 2.0 (:v state)))}))

(defn increase-iteration [state]
  (assoc state :counter (+ 1 (:counter state))))
    

(defn loop-levmar [state0]
  (loop [state state0]
    (cond
     (:stop state) state
     :default (let [stepped (compute-dx state)]
                (if (:stop stepped) stepped
                    (if (minimum-step-size? stepped)
                      (stop stepped :minstep)
                      (let [attempt (attempt-step stepped)]
                        (if (improvement? attempt)
                          (accept-state attempt)
                          (recur (increase-damping attempt))))))))))

(defn should-loop? [state]
  (let [settings (:settings state)]
    (not (or (< (max-abs-element (:JtF state)) (:e1 settings))
             (< (:norm2F state) (:e3 settings))))))
     

(defn take-levmar-step [state]
  (assert (= 1 (dimensionality (:X state))))
  (if (>= (:counter state) (:maxiter (:settings state)))
    (stop state :maxiter)
    (increase-iteration 
     (let [local-state (make-local-state state)]
       (if (should-loop? local-state)
         (loop-levmar local-state)
         (stop local-state :f-close-to-0))))))

; Call this to reset certain things
(defn reset-state [state]
  (merge state
         {:stop false
          :counter 0}))



(defn add-num-jac-if-needed
  ([problem] (add-num-jac-if-needed problem 1.0e-6))
  ([problem h]
   (let [factor (/ 1.0 (* 2.0 h))]
     (if (and (contains? problem :objf) (not (contains? problem :objf-j)))
       (assoc problem :objf-j
              (fn [X]
                (assert (= 1 (dimensionality X)))
                (let [cols (dimension-count X 0)
                      F (let [temp ((:objf problem) X)]
                          (assert (= 1 (dimensionality temp)))
                          temp)
                      rows (dimension-count F 0)
                      gradients (scopedisp (vec
                                 (map
                                  (fn [index]
                                    (value-of X)
                                    (scopedisp
                                     (let [xplus (scopedisp (mset X index (+ (mget X index) h)))
                                          xminus (scopedisp (mset X index (- (mget X index) h)))
                                          fplus (scopedisp ((:objf problem) xplus))
                                          fminus (scopedisp ((:objf problem) xminus))]
                                      (value-of X xplus xminus fplus fminus)
                                      (scopedisp (* factor (scopedisp (- fplus fminus)))))))
                                  (range cols))))]
                  [F (compute-matrix
                      [rows cols]
                      (fn [i j]
                        (value-of gradients)
                        (value-of i j)
                        (value-of (mget (value-of (nth gradients j)) i))))])))
       problem))))
                                   

; Add settings and numeric Jacobian, if needed.
(defn make-full [problem]
  (assert (map-with-keys? problem :initX :objf))
  (add-num-jac-if-needed
   (merge

    ; Fill in default settings
    {:settings settings}
    
    ; And here is the problem
    problem)))



(defn make-full-state [problem]
  (let [full-problem (make-full problem)]
    (merge
     full-problem
     (make-levmar-state
      (:initX full-problem)))))

(defn run-levmar [problem]
  (loop [state (make-full-state problem)]
    (if (:stop state) state
        (recur (take-levmar-step state)))))

(defn make-demo-problem []
  {:objf (fn [X] (:F (test-objf X)))
   :initX [30 30]})

(defn run-demo []
  (run-levmar (make-demo-problem)))

(defn circle-interction-demo []
  (run-levmar
   {:objf (fn [X] [(- (norm (- X [0 0])) 1)
                   (- (norm (- X [1 0])) 1)])
    :initX [30 1]}))

(comment

  (do
    (def s0 (make-local-state test-objf (make-levmar-state [30 30]) settings))
    (def s1 (compute-dx s0))
    (def s2 (attempt-step test-objf s1))
    (def s3 (accept-state s2)))

  )
                  
                  
