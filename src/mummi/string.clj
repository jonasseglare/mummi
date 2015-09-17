(ns mummi.string)

(defn begins-with [a b]
  (if (>= (count a) (count b))
    (= (subs a 0 (count b))
       b)))

(defn string-to-data [s]
  (assert (string? s))
  (map read-string
       (filter (fn [x] (not (empty? x)))
               (clojure.string/split s #"\s+"))))

(defn has-ordered-words? [words s]
  (re-matches
   (re-pattern (str ".*" (clojure.string/join ".*" words) ".*"))
   s))
