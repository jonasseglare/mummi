(ns mummi.math.infinomial
  (:require [mummi.debug :as debug])
  (:require [mummi.math.bindings :as bds]))

(defn get-weight [x]
  (if (vector? x)
    (first x) x))

(defn get-exponent [x]
  (if (vector? x)
    (second x) 0))

(defn make-infinomial [weight exponent]
  (if (= 0 exponent) weight
      [weight exponent]))

(defn get-weight-and-exponent [x]
  [(get-weight x) (get-exponent x)])

(defn infinomial-abs [x]
  (make-infinomial
   (Math/abs (get-weight x))
   (get-exponent x)))

(defn infinomial-neg [x]
  (make-infinomial
   (- (get-weight x))
   (get-exponent x)))

(defn infinomial-add [a b]
  (let [[aw ae] (get-weight-and-exponent a)
        [bw be] (get-weight-and-exponent b)]
    (if (= ae be)
      (make-infinomial (+ aw bw) ae)
      (if (< ae be) b a))))

(defn infinomial-mul [a b]
  (let [[aw ae] (get-weight-and-exponent a)
        [bw be] (get-weight-and-exponent b)]
    (make-infinomial
     (* aw bw) (+ ae be))))

(defn infinomial-div [a b]
  (let [[aw ae] (get-weight-and-exponent a)
        [bw be] (get-weight-and-exponent b)]
    (make-infinomial
     (/ aw bw) (- ae be))))

(defn infinomial-sub [a b]
  (infinomial-add
   a (infinomial-neg b)))


(defn infinomial-less? [a b]
  (let [[aw ae] (get-weight-and-exponent a)
        [bw be] (get-weight-and-exponent b)]
    (cond
      (or (= 0 aw) (= 0 bw)) (< aw bw)
      (< aw 0) (if (< bw 0)
                 (infinomial-less? (infinomial-neg b) (infinomial-neg a))
                 true)
      (< bw 0) false
      ;; At this stage, both aw and bw are positive
      
      (= ae be) (< aw bw)
      :default (< ae be))))
      
