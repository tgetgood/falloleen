(ns falloleen.renderer.canvas
  #?(:clj (:import [falloleen.lang
                    AffineWrapper
                    Arc
                    Bezier
                    Circle
                    ClosedSpline
                    Line
                    RawText
                    Spline
                    Style]))
  (:require [clojure.string :as string]
            [falloleen.lang :as lang :refer
             #?(:clj []
                :cljs [Arc
                       AffineWrapper
                       Bezier
                       Circle
                       ClosedSpline
                       Line
                       RawText
                       Spline
                       Style])]
            [falloleen.math :as math]
            [falloleen.util :include-macros true]
            [net.cgrand.macrovich :as macros :include-macros true]))

(defn convert-styles [{:keys [stroke fill opacity font]}]
  (into []
        (remove nil? [(when stroke
                        [:stroke (if (= :none stroke) :none (name stroke))])
                      (when fill
                        [:fill (if (= :none fill) :none (name fill))])
                      (when opacity [:opacity opacity])
                      (when font [:font font])])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Semi-abstract language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Canvas2DRenderable
  (cmds [this] "Returns a map of cmds to draw in various circumstances."))

(falloleen.util/implement-sequentials
 Canvas2DRenderable
 (cmds [this]
   (mapcat cmds this)))

(extend-protocol Canvas2DRenderable
  nil
  (cmds [_]
    (println "Can't render nil. Doing nothing."))

  #?(:cljs default :clj Object)
  (cmds [this]
    (if (lang/template? this)
      (cmds (lang/expand-template this))
      (println "Don't know how to render a "
               (type this) ". Doing nothing.")))

  AffineWrapper
  (cmds [this]
    (let [[a b c d x y] (lang/aw-matrix this)
          shape (.-shape this)]
      (conj
       (into
        [:save
         [:transform a b c d x y]]
        (cmds shape))
       :restore)
      ))

  RawText
  (cmds [{:keys [text]}]
    [[:fill-text text 0 0]])

  Line
  (cmds [{[x1 y1] :from [x2 y2] :to}]
    [:begin-path
     [:move-to x1 y1]
     [:line-to x2 y2]
     :end-curve])

  Arc
  (cmds [{r :radius [x y] :centre :keys [from to clockwise?] :as this}]
    (let [[x1 y1] (first (lang/boundary this))]
      [:begin-path
       [:move-to x1 y1]
       [:arc x y r from to (boolean clockwise?)]
       :end-curve]))

  Bezier
  (cmds [{[x1 y1] :from [x2 y2] :to [cx1 cy1] :c1 [cx2 cy2] :c2}]
    [:begin-path
     [:move-to x1 y1]
     [:bezier-curve-to cx1 cy1 cx2 cy2 x2 y2]
     :end-curve])

  Circle
  (cmds [{[x y] :centre r :radius}]
    [:begin-path
     [:arc x y r 0 (* 2 math/pi) true]
     :end-figure])

  Spline
  (cmds [{:keys [segments]}]
    (let [[x y] (first (lang/boundary (first segments)))]
      (conj (transduce (comp (map cmds)
                             cat
                             (remove #(contains? #{:begin-path :end-curve} %))
                             (remove #(= (first %) :move-to)))
                       conj
                       [:begin-path
                        [:move-to x y]]
                       segments)
            :end-curve)))

  ClosedSpline
  (cmds [{:keys [segments]}]
    (let [[x y] (first (lang/boundary (first segments)))]
      (conj (transduce (comp (map cmds)
                             cat
                             (remove #(contains? #{:begin-path :end-curve} %))
                             (remove #(= (first %) :move-to)))
                       conj
                       [:begin-path
                        [:move-to x y]]
                       segments)
            :end-figure)))

  Style
  (cmds [{:keys [style base]}]

    (into [:save]
          (concat
           (convert-styles style)
           (cmds base)
           [:restore]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Styling Stack Machine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-styles
  {:fill      "rgba(0,0,0,1)"
   :stroke    "rgba(0,0,0,1)"
   :opacity   1
   :font      "10px sans-serif"
   :transform [1 0 0 1 0 0]})

(def init-stack
  (list {:fill      nil
         :stroke    nil
         :opacity   nil
         :font      nil
         :transform [1 0 0 1 0 0]}))

(defn push-down [stack]
  (cons (first stack) stack))

(defn pop-frame [stack]
  (rest stack))

(defn diff-top [stack]
  (if (< (count stack) 2)
    (first stack)
    (let [f0 (first stack)
          f1 (second stack)]
      (into {}
            (map (fn [[k v]]
                   (when-not (= v (get f0 k))
                     [k v])))
            f1))))

(defn free? [stack p]
  (not (get (first stack) p)))

(defn assoc-style [stack & kvs]
  (cons (apply assoc (first stack) kvs) (rest stack)))

(defmulti run-stack! (fn [i ctx stack] (if (keyword? i) i (first i))))

(defmethod run-stack! :default
  [i _ stack]
  (println "Unknown instruction " i ", Skipping.")
  stack)

(defmethod run-stack! :save
  [_ ctx stack]
  (push-down stack))

(defmethod run-stack! :restore
  [_ ctx stack]
  (let [diff (diff-top stack)]
    (run! (fn [[k v]]
            (if (= :transform k)
              (let [[a b c d x y] (or v [1 0 0 1 0 0])]
                (.setTransform ctx a c b d x y))
              (if v
                (run-stack! [k v] ctx {})
                (run-stack! [k (get default-styles k)] ctx {}))))
          diff)
    (pop-frame stack)))

(defmethod run-stack! :begin-path
  [_ ctx stack]
  (.beginPath ctx)
  stack)

(defmethod run-stack! :end-curve
  [_ ctx stack]
  (.stroke ctx)
  stack)

(defmethod run-stack! :end-figure
  [_ ctx stack]
  (let [frame (first stack)]
    (.stroke ctx)
    (when (:fill frame)
      (.fill ctx))
    stack))

(defmethod run-stack! :move-to
  [[_ x y] ctx stack]
  (.moveTo ctx x y)
  stack)

(defmethod run-stack! :line-to
  [[_ x y] ctx stack]
  (.lineTo ctx x y)
  stack)

(defmethod run-stack! :fill-text
  [[_ text x y] ctx stack]
  (.fillText ctx text x y)
  stack)

(defmethod run-stack! :bezier-curve-to
  [[_ cx1 cy1 cx2 cy2 x2 y2] ctx stack]
  (.bezierCurveTo ctx cx1 cy1 cx2 cy2 x2 y2)
  stack)

(defmethod run-stack! :arc
  [[_ x y r from to c] ctx stack]
  (.arc ctx x y r from to c)
  stack)

(defmethod run-stack! :transform
  [[_ a b c d x y :as i] ctx stack]
  (let [t (get (first stack) :transform)
        m (get (first stack) :line-width 1)
        nm (/ m (falloleen.util/magnitude a b c d))
        nt (math/comp-atx t (rest i))]
    (.transform ctx a c b d x y)
    #?(:cljs (unchecked-set ctx "lineWidth" nm))
    (assoc-style stack
                 :transform nt
                 :line-width nm)))

(defmethod run-stack! :line-width
  [[_ m] ctx stack]
  #?(:cljs (unchecked-set ctx "lineWidth" m))
  stack)

(defmethod run-stack! :stroke
  [[_ m] ctx stack]
  (if (free? stack :stroke)
    (let [p (if (= m :none) "rgba(0,0,0,0)" m)]
      #?(:cljs (unchecked-set ctx "strokeStyle" p))
      (assoc-style stack :stroke m))
    stack))

(defmethod run-stack! :fill
  [[_ m] ctx stack]
  (if (free? stack :fill)
    (let [p (if (= m :none) "rgba(0,0.0,0)" m)]
      #?(:cljs (unchecked-set ctx "fillStyle" p))
      (assoc-style stack :fill m))
    stack))

(defmethod run-stack! :font
  [[k m] ctx stack]
  (if (free? stack :font)
    (do
      #?(:cljs (unchecked-set ctx "font" m))
      (assoc-style stack :font m))
    stack))

(defmethod run-stack! :opacity
  [[_ m] ctx stack]
  (if (free? stack :opacity)
    (do
      #?(:cljs (unchecked-set ctx "globalAlpha" m))
      (assoc-style stack :opacity m))
    stack))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Render
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- clear-screen!
  "Clear the rendering context's canvas."
  [ctx]
  (.clearRect ctx 0 0 (-> ctx .-canvas .-width) (-> ctx .-canvas .-height)))

(defn simple-render [shape ctx]
  (clear-screen! ctx)
  (reduce (fn [acc x] (run-stack! x ctx acc))
          init-stack
          (cmds shape)))
