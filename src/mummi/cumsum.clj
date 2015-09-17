(ns mummi.cumsum)

(defn calc-cumsum [x]
  (loop [remain x
         sum 0
         result [0]]
    (if (empty? remain)
      result
      (let [next-sum (+ sum (first remain))]
        (recur
         (rest remain)
         next-sum
         (conj result next-sum))))))

(defn lookup-index-cumsum [cumsum value]
  (let [n (count cumsum)]
    (if (= n 1) 0
        (let [middle (int (/ (count cumsum) 2))
              middle-value (nth cumsum middle)]
          (if (< value middle-value)
            (lookup-index-cumsum
             (subvec cumsum 0 middle) value)
            (+ middle
               (lookup-index-cumsum
                (subvec cumsum middle) value)))))))

(defn lookup-index-prop [props value]
  (lookup-index-cumsum
   (calc-cumsum props) value))

(defn sample-prop [props]
  (let [cumsum (calc-cumsum props)
        index (lookup-index-cumsum cumsum (rand (last cumsum)))]
    (if (and (<= 0 index)
             (< index (count props)))
      index)))

