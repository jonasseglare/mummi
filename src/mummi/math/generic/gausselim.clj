(ns mummi.math.generic.gausselim
  (:require [mummi.math.infinomial :refer :all])
  (:require [mummi.debug :as debug]))


(defn get-elim-factor [col row-a row-b]
  (infinomial-neg
   (infinomial-div
    (nth row-b col)
    (nth row-a col))))

(defn normalize-row [col row]
  (let [x (nth row col)]
    (map
     (fn [y]
       (infinomial-div y x))
     row)))
  
(defn perform-row-op [factor row-a row-b]
  (map
   (fn [a b]
     (infinomial-add
      (infinomial-mul factor a) b))
   row-a row-b))

(defn eliminate-row [col src-row dst-row]
  (perform-row-op
   (get-elim-factor col src-row dst-row)
   src-row dst-row))

(defn get-max-row-index [col rows]
  (first
   (reduce
    (fn [a b]
      (if (infinomial-less? (second a) (second b))
        b a))
    (map
     (fn [index row]
       [index (nth row col)])
     (range (count rows))
     rows))))

(defn range-but-index [n index]
  (map
   (fn [i]
     (if (< i index) i (+ 1 i)))
   (range (- n 1))))

(defn perform-elim [max-index col src-row A]
  (map
   (fn [index]
     (eliminate-row col src-row (nth A index)))
   (range-but-index (count A) max-index)))

(defn perform-elim-back [A]
  (let [src-row (last A)]
    (map
     (fn [dst-row]
       (eliminate-row
        (- (count A) 1) src-row dst-row))
     (butlast A))))
     

(defn forward-elim
  ([A] (forward-elim 0 A))
  ([col A]
   (if (empty? A) '()
       (let [index (get-max-row-index col A)
             normalized (normalize-row col (nth A index))]
         (cons
          normalized
          (forward-elim (+ 1 col) (perform-elim index col normalized A)))))))

;(defn backward-elim [A]
;(forward-elim 0 [[4 0] [1 5]])

(defn backward-elim [A]
  (if (empty? A) A
      (conj
       (vec (perform-elim-back A)) (last A))))

(defn split-atx [i A]
  [(map (fn [row] (subvec (vec row) 0 i)) A)
   (map (fn [row] (subvec (vec row) i)) A)])

(defn mat-hcat [A B]
  (vec (map (fn [a b] (vec (concat a b))) A B)))

(defn gauss-elim
  ([A] (backward-elim (forward-elim A)))
  ([A B] (split-atx (count (first A)) (gauss-elim (mat-hcat A B)))))

(defn is-matrix? [x]
  (when (vector? x)
    (vector? (first x))))

(defn to-matrix [x]
  (vec (map vector x)))

(defn to-vector [x]
  (map
   first
   x))

(defn solve [A B]
  (if (is-matrix? B)
    (second (gauss-elim A B))
    (to-vector (solve A (to-matrix B)))))
   
  
         

