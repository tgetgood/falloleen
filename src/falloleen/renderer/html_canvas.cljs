(ns falloleen.renderer.html-canvas
  (:require [falloleen.math :as math]
            [falloleen.renderer.canvas-compiler :as compiler]))

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
    (unchecked-set ctx "lineWidth" nm)
    (assoc-style stack
                 :transform nt
                 :line-width nm)))

(defmethod run-stack! :line-width
  [[_ m] ctx stack]
  (unchecked-set ctx "lineWidth" m)
  stack)

(defmethod run-stack! :stroke
  [[_ m] ctx stack]
  (if (free? stack :stroke)
    (let [p (if (= m :none) "rgba(0,0,0,0)" m)]
      (unchecked-set ctx "strokeStyle" p)
      (assoc-style stack :stroke m))
    stack))

(defmethod run-stack! :fill
  [[_ m] ctx stack]
  (if (free? stack :fill)
    (let [p (if (= m :none) "rgba(0,0.0,0)" m)]
      (unchecked-set ctx "fillStyle" p)
      (assoc-style stack :fill m))
    stack))

(defmethod run-stack! :font
  [[k m] ctx stack]
  (if (free? stack :font)
    (do
      (unchecked-set ctx "font" m)
      (assoc-style stack :font m))
    stack))

(defmethod run-stack! :opacity
  [[_ m] ctx stack]
  (if (free? stack :opacity)
    (do
      (unchecked-set ctx "globalAlpha" m)
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
          (compiler/cmds shape)))
