(ns mummi.gui.gridbag
  (:import [java.awt GridBagLayout GridBagConstraints Insets])
  (:import [javax.swing JPanel])
  (:require [mummi.debug :as debug])
  (:require [mummi.common :as common]))

(def fill-map
  {:none GridBagConstraints/NONE
   :horizontal GridBagConstraints/HORIZONTAL
   :vertical GridBagConstraints/VERTICAL
   :both GridBagConstraints/BOTH})

(defn fill-value? [x]
  (contains? fill-map x))

;; FIRST_LINE_START 	PAGE_START 	FIRST_LINE_END
;; LINE_START 	CENTER 	LINE_END
;; LAST_LINE_START 	PAGE_END 	LAST_LINE_END

(def anchor-map
  {:first-line-start GridBagConstraints/FIRST_LINE_START
   :page-start GridBagConstraints/PAGE_START
   :first-line-end GridBagConstraints/FIRST_LINE_END
   :line-start GridBagConstraints/LINE_START
   :center GridBagConstraints/CENTER
   :line-end GridBagConstraints/LINE_END
   :last-line-start GridBagConstraints/LAST_LINE_START
   :page-end GridBagConstraints/PAGE_END
   :last-line-end GridBagConstraints/PAGE_END})

(defn anchor-value? [x]
  (contains? anchor-map x))

(def basic-inset-keywords #{:left :right :top :bottom})

(def inset-keywords
  (clojure.set/union
   basic-inset-keywords
   #{:vertical :horizontal :any}))

(def initial-insets
  (zipmap basic-inset-keywords (take (count basic-inset-keywords) (repeat 0))))

(defn apply-inset [src-keyword dst-keywords dst-inset-map]
  (if-let [value (get dst-inset-map src-keyword)]
    (dissoc
     (loop [result dst-inset-map
            remaining dst-keywords]
       (if (empty? remaining)
         result
         (recur (assoc result (first remaining) value)
                (rest remaining))))
     src-keyword)
    dst-inset-map))

(defn build-insets [m]
  (apply-inset
   :horizontal  [:left :right]
   (apply-inset
    :vertical [:top :bottom]
    (apply-inset
     :any basic-inset-keywords
     (merge initial-insets m)))))

(defn build-insets-obj [m0]
  (let [m (build-insets m0)]
    (Insets. (:top m) (:left m) (:bottom m) (:right m))))

(def default-constraints
  {:widget nil
   :gridx nil
   :gridy nil
   :gridwidth 1
   :gridheight 1
   :fill :none
   :ipadx 0
   :ipady 0
   :insets {}
   :anchor :center
   :weightx 0
   :weighty 0})

(def default-padding 8)

(defn merge-cst [a b]
  (assoc
   (merge a b)
   :insets (merge (build-insets (:insets a))
                  (:insets b))))

(defn check-constraints [x]
  (common/validate-data map? x)
  (common/validate-data identity (:widget x))
  (common/validate-data number? (:gridx x))
  (common/validate-data number? (:gridy x))
  (common/validate-data number? (:gridwidth x))
  (common/validate-data number? (:gridheight x))
  (common/validate-data fill-value? (:fill x))
  (common/validate-data number? (:ipadx x))
  (common/validate-data number? (:ipady x))
  (common/validate-data map? (:insets x))
  (common/validate-data anchor-value? (:anchor x))
  (common/validate-data number? (:weightx x))
  (common/validate-data number? (:weighty x)))

(defn build-constraints [item]
  (let [cst (GridBagConstraints.)]
    (set! (.gridx cst) (:gridx item))
    (set! (.gridy cst) (:gridy item))
    (set! (.gridwidth cst) (:gridwidth item))
    (set! (.gridheight cst) (:gridheight item))
    (set! (.fill cst) (fill-map (:fill item)))
    (set! (.ipadx cst) (:ipadx item))
    (set! (.ipady cst) (:ipady item))
    (set! (.insets cst) (build-insets-obj (:insets item)))
    (set! (.anchor cst) (anchor-map (:anchor item)))
    (set! (.weightx cst) (:weightx item))
    (set! (.weighty cst) (:weighty item))
    cst))
    
(defn make-gridbag-panel
  ([cst0 items]
   (common/validate-data map? cst0)
   (let [cst (merge-cst default-constraints cst0)
         panel (doto (JPanel.)
                 (.setLayout (GridBagLayout.)))]
     (doseq [item0 items]
       (let [item (merge-cst cst item0)]
         (check-constraints item)
         (.add
          panel
          (:widget item)
          (build-constraints item))))
     panel))
  ([items]
   (make-gridbag-panel default-constraints items)))
    
   

;;;;; Extras
(defn padded-items [padding items orientation]
  (common/validate-data number? padding)
  (common/validate-data common/vecseq? items)
  (make-gridbag-panel
   {:insets {:any padding}}
   (map
    (fn [index]
      (merge (if (= :vertical orientation)
               {:gridx 0 :gridy index :insets (if (= 0 index) {} {:top 0})}
               {:gridx index :gridy 0 :insets (if (= 0 index) {} {:left 0})})
             {:widget (nth items index)}))
    (range (count items)))))

(defn padded-bar
  ([padding items]
   (padded-items padding items :horizontal))
  ([items] (padded-bar default-padding items)))

(defn padded-stack
  ([padding items]
   (padded-items padding items :vertical))
  ([items]
   (padded-stack default-padding items)))
   
