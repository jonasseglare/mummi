(ns mummi.math.ops
  (:require [mummi.math.generic.gausselim :as gausselim])
  (:require [mummi.debug :as debug :refer [value-of]])
  (:require [mummi.common :refer [min-val-and-index
                                  pair-with-indices
                                  min-inds]])
  (:require [clojure.pprint :refer :all])
  (:refer-clojure :exclude [* - + == /])
  (:require [mummi.math.utils :refer [numel empty-matrix?
                                      mat-to-vec vec-to-col]])
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all])

  ; To get solve
  (:require [clojure.core.matrix.linear :refer :all]))


(set-current-implementation :vectorz)

(def testmat [[1 2] [3 4] [5 6]])
(def cstmat (transpose testmat))

; Computes the nullspace of X, that is a matrix
; whose columns span the space of all vectors Y such that X*Y = 0:
; (mmul X (nullspace X)) should be close to 0.
(defn nullspace [X]
  (let [[from to] (shape X)]
    (assert (<= from to))
    (let [{:keys [Q R]} (qr (transpose X) {:compact false})]
      (assert Q)
      (assert R)
      (submatrix Q 1 [from (- to from)]))))


;; Temporary solution until this is supported by core.matrix
(defn solve-multi-rhs [A B]
  (gausselim/solve A B))

(defn least-squares-multi-rhs [A B]
  (solve-multi-rhs
   (mmul (transpose A) A)
   (mmul (transpose A) B)))

(defn any-solve-multi-rhs [A B]
  (if (square? A)
    (solve-multi-rhs A B)
    (least-squares-multi-rhs A B)))

(defn any-solve
  ([A B solver]
   (if (clojure.core.matrix/vec? B)
     (solver A B)
     (let [bshape (shape B)]
       (if (= 1 (second bshape))
         (vec-to-col
          (solver A (mat-to-vec B)))
         (any-solve-multi-rhs A B)))))
  ([A B]
   (any-solve A B solve)))

(defn any-mmul [arg & args]
  (let [narg (to-nested-vectors arg)]
    (if (empty? args)
      narg
      (mmul narg (apply any-mmul args)))))
    

; The particular solution of an under-determined linear system
(defn particular [A B]
  (mmul
   (transpose A)
   (any-solve (mmul A (transpose A)) B)))


(defn solve-with-cst
  ([A B Ac Bc]
   (if (or (empty-matrix? Ac) (empty-matrix? Bc))
     (if (or (empty-matrix? A) (empty-matrix? B)) []
         (any-solve A B))
     (let [Q (nullspace Ac)]
       (if (= 0 (numel Q))
         (any-solve Ac Bc)
         (do
           (let [q (particular Ac Bc)
                 AQ (mmul A Q)
                 BAq (- B (mmul A q))
                 lambda (any-solve AQ BAq)]
             (+ (any-mmul Q lambda) q))))))))

(defn solve-with-cst-soft
  ([A B Ac Bc w]
   (least-squares-multi-rhs
    (debug/value-of (concat
     A (* w Ac)))
    (debug/value-of (concat
     B (* w Bc)))))
  ([A B Ac Bc]
   (solve-with-cst-soft A B Ac Bc 1.0e6)))

(defn my-eigen [X]
  (let [{:keys [U S V*]} (svd X)] ; 
    {:Q (transpose V*)
     :A S}))

(defn min-eig-vec [X]
  (let [{:keys [A Q]} (my-eigen X); X = Q*A*Q', Q'*Q = identy
        index (second (min-val-and-index (eseq A)))]
    (submatrix Q 1 [index 1])))

; Min_{X with |X|=1} |A*X|^2
;
; Solve a homogeneous least squares problem
(defn homogeneous [A]
  (min-eig-vec (mmul (transpose A) A)))
  

(defn row-sum [matrix]
  (let [[rows cols] (shape matrix)]
    (mat-to-vec
     (mmul (compute-matrix [1 rows] (fn [i j] 1))
           matrix))))

(defn mean-row [matrix]
  (let [[rows cols] (shape matrix)]
    (* (/ 1.0 rows) (row-sum matrix))))

(defn mean-col [matrix]
  (mean-row (transpose matrix)))

(defn col-sum [matrix]
  (row-sum (transpose matrix)))

(defn pca [data-as-rows]
  (let [[n d] (shape data-as-rows)
        C (mean-row data-as-rows)
        centred (compute-matrix
                 [n d]
                 (fn [i j] (- (mget data-as-rows i j)
                              (mget C j))))
        {:keys [Q A]} (my-eigen (* (/ 1.0 n) (mmul (transpose centred)
                                                   centred)))
        vars A
        vecs Q]
    {:count n
     :dims d
     :cog C
     :vecs vecs
     :vars vars}))

(defn reduce-dimension [pts new-dims]
  (let [princomp (pca pts)
        sorted-inds (map second
                         (sort (fn [a b] (> (first a) (first b)))
                               (pair-with-indices (eseq (:vars princomp)))))
        fullb (:vecs princomp)
        basis (compute-matrix [(:dims princomp) new-dims]
                              (fn [i j]
                                (mget fullb i (nth sorted-inds j))))
        reduced (mmul pts basis)]
    {:original-pts pts
     :reduced-pts reduced
     :basis basis}))

(defn angle-between-vectors [X0 Y0]
  (let [X (mat-to-vec X0)
        Y (mat-to-vec Y0)]
    (Math/acos (/ (dot X Y) (* (norm X) (norm Y))))))

(defn make-basis
  ([A] (make-basis A (column-count A)))
  ([A cols]
   (let [{:keys [Q A]} (my-eigen A)
         inds (min-inds (eseq A))]
     (compute-matrix [(row-count Q) cols]
                     (fn [i j]
                       (mget Q i (nth inds j)))))))

(defn make-random-matrix
  ([shape] (make-random-matrix shape [0 1]))
  ([shape span]
   (let [n (count shape)]
     (compute-matrix shape
                     (fn [& args]
                       (+ (first span)
                          (rand (- (second span) (first span)))))))))

(defn make-random-vector
  ([n] (make-random-vector n [0 1]))
  ([n span]
   (vec (map (fn [i] (+ (first span) (rand (- (second span)
                                              (first span)))))
             (range n)))))


(defn drop-range [range index]
  (let [[from to] range]
    (cond
      (< index from) index
      (< index to) nil
      :default (- index (- to from)))))

(defn drop-range-inv [range index]
  (let [[from to] range]
    (if (< index from) index
        (+ index (- to from)))))

(defn back-and-forth [range x]
  (drop-range range (drop-range-inv range x)))

(defn compute-size-dropping-range [sh dim span]
  (let [[from to] span
        dif (- to from)]
    (update-in sh [dim] (fn [x] (- x dif)))))

(defn remap-inds-dropping-range-inv [inds dim span]
  (update-in
   (vec inds) [dim]
   (fn [index]
     (drop-range-inv span index))))

; (compute-size-dropping-range [4 9] 1 [3 5])

(defn drop-matrix-range [matrix dim range]
  (compute-matrix
   (compute-size-dropping-range (shape matrix) dim range)
   (fn [& inds]
     (mp/get-nd
      matrix
      (remap-inds-dropping-range-inv
       inds dim range)))))

(defn drop-slice [matrix dim index]
  (drop-matrix-range matrix dim [index (inc index)]))

(defn drop-column [matrix index]
  (drop-slice matrix 1 index))

(defn drop-row [matrix index]
  (drop-slice matrix 0 index))

;(drop-matrix-range [[0 1 2] [3 4 5]] 0 [0 1])

; Experiment: Find closest point to (x, y) = (3, 4) with y = 1.3
(comment

  (do
    (def A (identity-matrix 2))
    (def B [[3] [4]])
    (def Ac [[0 1]])
    (def Bc [[1.3]])
    (def Q (nullspace Ac))
    (def q (particular Ac Bc))
    (def X (value-of (solve-with-cst A B Ac Bc)))
  )
)
