(ns mummi.password)

(def capital-letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def small-letters   "abcdefghijklmnopqrstuvwxyz")
(def digits "0123456789")
(def special "!#%&/()=?@£$[]}\\{")

(defn sample-symbols [n symbols]
  (apply
   str
   (map (fn [i] (nth symbols (rand-int (count symbols))))
        (range n))))

(defn sample-by-groups [& count-symbol-pairs]
  (apply
   str
   (shuffle
    (seq
     (apply
      str
      (map (fn [pair]
             (let [[n symbols] pair]
               (sample-symbols n symbols)))
           count-symbol-pairs))))))
