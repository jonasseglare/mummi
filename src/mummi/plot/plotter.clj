(ns mummi.plot.plotter
  (:require [mummi.signe.controller :as controller])  
  (:require [mummi.math.utils :refer [mat-to-vec vec-to-col]])
  (:require [mummi.geometry.line :refer :all])
  (:require [mummi.common :refer [defrecord2 defprotocol2 with-field clamp]])
  (:require [mummi.geometry.bbox :as bbox])
  (:require [mummi.log :refer [log-message]])
  (:require [mummi.math.affine :refer [apply-affine fit-affine-translate]])
  (:require [mummi.debug :refer [value-of]])
  (:require [mummi.image :as image])
  (:require [clojure.pprint :refer :all])
  (:refer-clojure :exclude [* - + == /])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all])

  ; To get solve
  (:require [clojure.core.matrix.linear :refer :all]))

(set-current-implementation :vectorz)

(defn padded [bbox]
  (if (bbox/defined? bbox)
    (bbox/pad-bbox bbox)
    (bbox/make-image-bbox [1 1 1])))

;; Protocol for data to be plotted.
;; It should specify WHAT to plot, but doesn't specify how to plot it.
;; It could be extended to interact with the data through mouse events.
(defprotocol PlotCtrl
  (get-objects [this])
  (mouse-event-at [this event-type line]))

(defn PlotCtrl? [x]
  (and (satisfies? PlotCtrl x)
       (bbox/Bounded? x)))


(extend-type clojure.lang.IPersistentCollection
  PlotCtrl
  (get-objects [this]
    (vec this))
  (mouse-event-at [this event line]
    this))

(extend-type clojure.lang.IPersistentCollection
  bbox/Bounded
  (get-bbox [this] nil))


(def default-options
  {:line-width 2
   :color [0 0 1]
   :bg-color [1 1 1]
   :font 30
   :marker-size 6})


; Interface used when we plot things
(defprotocol Plotter
  "A low-level interface for objects that can be rendered"
  (plot-line [plotter from to options])
  (plot-marker [plotter position options])
  (plot-text [plotter position text options])
  (plot-image [plotter transform image])
  (combine [ploptter other-plotter]))

(defprotocol Plottable
  (render [this plotter])
  (get-plottable-bbox [this]))

(defn Plottable? [x]
  (and (satisfies? Plottable x)
       (bbox/Bounded? x)))

(defn get-options-from-vector [v]
  (merge
   default-options
   (if (map? (second v))
     (second v)
     {})))

(defn get-data-from-vector [v]
  (subvec v (if (map? (second v)) 2 1)))

(defn get-vec-opts-and-data [v]
  [(get-options-from-vector v) (get-data-from-vector v)])

(defn render-linestrip [v plotter]
  (let [[opts data] (get-vec-opts-and-data v)]
    (reduce
     combine
     (map
      (fn [src dst]
        (plot-line plotter src dst opts))
      (butlast data)
      (rest data)))))

(defn render-lines [v plotter]
  (let [[opts data] (get-vec-opts-and-data v)]
    (reduce
     combine
     (fn [pair]
       (let [[from to] pair]
         (plot-line plotter from to opts)))
     data)))

(defn render-point [v plotter]
  (let [[opts data] (get-vec-opts-and-data v)]
    (apply
     plot-marker
     (concat [plotter] data [opts]))))

(defn render-points [v plotter]
  (let [[opts data] (get-vec-opts-and-data v)]
    (reduce
     combine
     (map
      (fn [pt]
        (plot-marker plotter pt opts))
      data))))

(defn render-line [v plotter]
  (let [[opts data] (get-vec-opts-and-data v)
        [src dst] data]
    (plot-line plotter src dst opts)))

(defn render-text [v plotter]
  (let [[opts data] (get-vec-opts-and-data v)
        [text pos] data]
    (plot-text plotter text pos opts)
    plotter))

(defn render-image [v plotter]
  (let [[opts data] (get-vec-opts-and-data v)
        [image] data]
    (plot-image plotter (:transform opts) image)))

(defn render-vector [v plotter]
  (case (first v)
    :linestrip (render-linestrip v plotter)
    :points (render-points v plotter)
    :lines (render-lines v plotter)
    :point (render-point v plotter)
    :line (render-line v plotter)
    :image (render-image v plotter)
    :text (render-text v plotter)))


(defn calc-image-bbox-corners [T image]
  (let [c (map (fn [x] (conj x 0)) (image/get-corners image))]
    (if (nil? T) c (apply-affine T c))))

(defn get-bbox-from-vector [x]
  (let [[opts data] (get-vec-opts-and-data x)]
    (bbox/make-bbox
     (case (first x)
       :linestrip data
       :points data
       :lines (reduce concat data)
       :point [data]
       :line data
       :image (calc-image-bbox-corners (:transform opts) (first data))
       :text [(second data)]))))



(extend-type clojure.lang.IPersistentVector
  Plottable
  (render [this plotter]
    (render-vector this plotter))
  (get-plottable-bbox [this]
    (get-bbox-from-vector this)))

       





(defn render-many [plotter plot-objs]
  (doseq [obj plot-objs]
    (render obj plotter)))

(defrecord2 DebugPlotter [bbox lines markers])

(defn make-debug-plotter []
  (DebugPlotter. (bbox/make-bbox) [] []))

(extend-type DebugPlotter
  Plotter
  (get-bbox [plotter]
    (:bbox plotter))
  (is-mutable? [plotter] false)
  (plot-line [plotter from to options]
    (merge plotter
           {:lines (conj (:lines plotter) {:from from :to to :options options})
            :bbox (bbox/expand (:bbox plotter) (bbox/make-bbox [from to]))}))
  (plot-marker [plotter position options]
    (merge plotter
           {:markers (conj (:markers plotter)
                           {:position position :options options})
            :bbox (bbox/expand (:bbox plotter)
                               (bbox/make-bbox [position]))})))

(defprotocol PlotView
  "Represents the transform for viewing the plot"

  ; Maps a 2d or 3d position to
  ; the projected space
  (project [view x])

  ; Maps a projected position to a line of sight
  (get-line [view pos])

  ; Rotation, defined by a mouse movement in 2d.
  (change [view vec2d]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 3d plot view
(defrecord2 PlotView3d [phi theta proj])

(defn make-2d-vec [phi]
  [(cos phi) (sin phi)])

(defn make-3d-vec [phi theta]
  (let [[x y] (make-2d-vec phi)
        cost (Math/cos theta)]
    [(* cost x) (* cost y) (Math/sin theta)]))

(def alpha (* 0.25 Math/PI))

(defn make-plot-view-3d-proj-mat
  ([] (make-plot-view-3d-proj-mat alpha alpha))

  ; Viewing angles
  ([phi theta]
   (let [dir (make-3d-vec phi theta)
         up [0 0 1]
         z-axis (- dir)
         x-axis (normalise (cross up dir))
         y-axis (normalise (cross z-axis x-axis))]
     [x-axis y-axis z-axis])))

(defn make-plot-view-3d
  ([] (make-plot-view-3d alpha alpha))
  ([phi theta]
   (PlotView3d.
    phi
    theta
    (make-plot-view-3d-proj-mat phi theta))))

(defn resize-vec
  ([x desired-length fillval]
   (let [n (count x)]
     (vec
      (map (fn [i]
             (if (< i n) (nth x i) fillval))
           (range desired-length)))))
  ([x desired-length]
   (resize-vec x desired-length 0)))

(defn get-basis [plot-view]
  (transpose (:proj plot-view)))

(def half-pi (* 0.5 Math/PI))

(extend-type PlotView3d
  PlotView
  (project [view x]
    (mmul (:proj view)
          (resize-vec x 3)))
  
  (get-line [view pos]
    (let [b (get-basis view)]
      (make-line
        (vec
         (eseq
           (let [a (submatrix b 1 [0 2])]
             (mmul a
                   (vec-to-col pos)))))
        (vec (eseq (submatrix b 1 [2 1]))))))
    
  (change [view vec2d]
    (assert (= 2 (count vec2d)))
    (let [scaled vec2d
          phi (+ (first scaled) (:phi view))
          theta (clamp (+ (second scaled) (:theta view))
                       (- half-pi) half-pi)]
      (merge view
             {:phi phi
              :theta theta
              :proj (make-plot-view-3d-proj-mat phi theta)}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2d plot view
(defrecord PlotView2d [proj])

(defn make-plot-view-2d
  ([ij?]
   (PlotView2d.
    [[1 0]
     [0 (if ij? 1 -1)]]))
  ([] (make-plot-view-2d false)))

(extend-type PlotView2d
  PlotView
  (project [view x]
    (mmul (:proj view)
          (resize-vec x 2)))

  (get-line [view pos]
    (let [b (get-basis view)]
      (make-line
       (resize-vec (mmul b pos) 3)
       [0 0 1])))

  (change [view vec2d] view))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2d transformations
(defn transform-2d [M x0]
  ; Drop extra dimensions
  (let [x (resize-vec x0 2)]
    (vec
     (eseq
      (let [s (shape M)]
        (if (= [2 3] s)
          (mmul M (resize-vec x 3 1))
          (do (assert (= [2 2] s))
              (mmul M x))))))))

(defn invert-transform-2d [M]
  (clone
   (let [s (shape M)]
    (if (= s [2 2])
      (inverse M)
      (do (assert (= s [2 3]))
          (let [Ainv (inverse (submatrix M 1 [0 2]))
                B (submatrix M 1 [2 1])]
            (join-along 1 Ainv (- (mmul Ainv B)))))))))

(defn make-transform-2d [scaling translate]
  (join-along 1
              (* scaling (identity-matrix 2))
              (vec-to-col translate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fits a 2d transformation based on bounding boxes

(defn project-bbox [plot-view bbox]
  (bbox/make-bbox
   (map (fn [corner]
          (project plot-view corner))
        (bbox/get-corners bbox))))

(defn fit-view
  ([src-bbox dst-bbox]
   (assert (bbox/BBox? src-bbox))
   (assert (bbox/BBox? dst-bbox))
   (assert (= (bbox/dims? src-bbox)
              (bbox/dims? dst-bbox)))
   (let [src-c (bbox/center src-bbox)
         dst-c (bbox/center dst-bbox)
         scaling (reduce
                  min
                  (map /
                       (bbox/size dst-bbox)
                       (bbox/size src-bbox)))
         translate (- dst-c (* scaling src-c))]
     (make-transform-2d scaling translate)))
  ([bbox plot-view dst-bbox]
   (fit-view (bbox/map-bbox (fn [v3]
                              (resize-vec v3 2))
                            (project-bbox plot-view bbox))
             dst-bbox)))


;;bbox dst-size user-view fit-view plot-view objects
(defn apply-fit-view [vm]
  (assoc
   vm :fit-view
   (fit-view (padded (:bbox vm))
             (:plot-view vm)
             (bbox/make-image-bbox (:dst-size vm)))))
         
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Final user scaling
(defrecord2 UserView [scale translate transform])

(defn make-user-view
  ([] (make-user-view 1 [0 0]))
  ([scale translate] (UserView. scale translate
                                (make-transform-2d scale translate))))


(defn scale-user-view [uv dst-size scale]
  (let [y (/ dst-size 2)
        x (/ (- y (:translate uv)) (:scale uv))
        new-scale (* (:scale uv) scale)]
    (make-user-view
     new-scale
     (- y (* new-scale x)))))

(defn translate-user-view [uv translate]
  (assert (UserView? uv))
  (make-user-view (:scale uv) (+ (:translate uv)
                                 translate)))
                    
        


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; View manager
; Commands to implement later, in swing.
;  Ctrl + left mouse: Rotate
;  Ctrl + right mouse: Translate
;  Mousewheel: Zoom in/out
(defrecord2 ViewManager [bbox dst-size user-view fit-view plot-view plot opts])

(defn calc-plotobj-bbox [plot]
  (assert (PlotCtrl? plot))
  (if-let [bbox (bbox/get-bbox plot)]
    bbox
    (bbox/make-bbox
     (let [objs (get-objects plot)]
       (if (empty? objs)
         [[0 0 0] [1 1 1]]
         (map
          get-plottable-bbox
          objs))))))


(defn refresh-bbox [vm]
  (assert (ViewManager? vm))
  (assoc vm :bbox (calc-plotobj-bbox (:plot vm))))

(defn refresh [vm]
  (apply-fit-view
   (refresh-bbox vm)))

(defn make-view-manager
  ([dst-size plot-view] (make-view-manager dst-size plot-view []))
  ([dst-size plot-view plot]
   (assert (vector? dst-size))
   (assert (= 2 (count dst-size)))
   (refresh
    (ViewManager.
     nil
     dst-size
     (make-user-view)
     nil
     plot-view
     plot
     default-options))))


(def default-dst-size [640 480])

(defn make-view-manager-2d
  ([] (make-view-manager-2d default-dst-size))
  ([dst-size] (make-view-manager-2d [] dst-size))
  ([objects dst-size]
   (make-view-manager dst-size
                      (make-plot-view-2d)
                      objects)))

(defn make-view-manager-3d
  ([] (make-view-manager-3d default-dst-size))
  ([dst-size] (make-view-manager-3d [] dst-size))
  ([objects dst-size]
   (make-view-manager dst-size
                      (make-plot-view-3d)
                      objects)))

;;;;;; Project points with this
(defn vm-project [view-manager pt]
  (transform-2d
   (:transform (:user-view view-manager))
   (transform-2d
    (:fit-view view-manager)
    (project (:plot-view view-manager) pt))))

(defn vm-get-affine [vm]
  (let [src [[0 0 0] [0 0 1] [0 1 0] [1 0 0]]
        dst (map (fn [x] (vm-project vm x)) src)]
    (fit-affine-translate src dst)))
    

;;;;;; Retrieve the line with this
(defn vm-get-line [view-manager pt]
  (get-line
   (:plot-view view-manager)
   (transform-2d
    (invert-transform-2d (:fit-view view-manager))
    (transform-2d
     (invert-transform-2d (:transform (:user-view view-manager)))
     pt))))



;;;;;; MODIFYING THE VM

(defn valid-size? [s]
  (< 0 (reduce * s)))

; When we resize the window
(defn set-vm-dst-size [vm dst-size]
  (if (not (valid-size? dst-size))
    vm
    (apply-fit-view
     (assoc vm :dst-size dst-size))))

; When we change the data to be plotted
(defn set-vm-bbox [vm bbox]
  (assert (ViewManager? vm))
  (assert (bbox/BBox? bbox))
  (apply-fit-view
   (assoc vm :bbox bbox)))

(defn get-ref-length [vm]
  (* 0.2 (bbox/diag-length (padded (:bbox vm)))))

(defn vm-set-objects [vm plot]
  (refresh (assoc vm :plot plot)))

; When modifying the user view
(defn vm-reset-user-view [vm]
  (assoc vm :user-view
         (make-user-view)))

(defn vm-scale-user-view [vm scale]
  (assoc vm :user-view
         (scale-user-view (:user-view vm) (:dst-size vm) scale)))

(defn vm-translate-user-view [vm translate]
  (assert (ViewManager? vm))
  (assoc vm :user-view
         (translate-user-view (:user-view vm) translate)))

(defn vm-rotate-plot-view [vm disp]
  (apply-fit-view
   (with-field vm plot-view
     (change plot-view disp))))

(defn vm-set-plot-view [vm new-pv]
  (apply-fit-view
   (with-field vm plot-view new-pv)))

(defn draw-axes [plotter vm]
  (let [rl (get-ref-length vm)
        opts (assoc default-options
               :color [0 0 0])]
    (doseq [i (range 3)]
      (plot-line plotter
                 [0 0 0]
                 (assoc [0 0 0] i rl)
                 opts))))
                 
    
(defn vm-ctrl? [x]
  (if (controller/Controller? x)
    (ViewManager? (controller/get-state x))))


(defn make-vm-ctrl
  ([vm plot-ctrl]
   (controller/derive-controller
    (controller/compose
     {:vm (controller/make-controller vm)
      :plot (controller/make-controller plot-ctrl)})
    (fn [c]
      (vm-set-objects
       (:vm c) (:plot c)))
    (fn [x c]
      {:vm c
       :plot (:plot x)}))))


(defn set-2d-ij [vm]
  (vm-set-plot-view (make-plot-view-2d true)))

(defn set-2d-xy [vm]
  (vm-set-plot-view (make-plot-view-2d false)))
