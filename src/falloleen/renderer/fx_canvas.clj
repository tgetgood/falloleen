(ns falloleen.renderer.fx-canvas
  (:require [falloleen.lang :as lang]
            [falloleen.util :as util])
  (:import [falloleen.lang AffineWrapper Bezier Circle Line Spline Style]
           javafx.scene.canvas.GraphicsContext))

;; GO GO dynamically scoped globals!!!
(def ^:dynamic *state* {})

(defn fill? []
  (when-let [f (:fill *state*)]
    (not= :none f)))

(defn stroke? []
  (let [s (:stroke *state*)]
    (not= ::none s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Inefficient and easy method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol DirectExecutor
  "Shapes implementing this protocol can directly draw themselves to screen in
  recursive decent style."
  (draw! [this ctx]))

(defprotocol InPathExecutor
  "Shapes which can be drawn as boundaries of regions."
  (draw-path! [this ctx]))

(util/implement-sequentials DirectExecutor
  (draw! [this ctx]
    (run! #(draw! % ctx) this)))

(extend-protocol InPathExecutor
  Object
  (draw-path! [this ctx]
    (println "I don't know how to draw a" (type this) "in a path."))

  Line
  (draw-path! [{[x1 y1] :from [x2 y2] :to} ^GraphicsContext ctx]
    (.moveTo ctx x1 y1)
    (.lineTo ctx x2 y2))

  Bezier
  (draw-path! [{[x1 y1] :from [cx1 cy1] :c1 [cx2 cy2] :c2 [x2 y2] :to}
               ^GraphicsContext ctx]
      (.moveTo ctx x1 y1)
      (.bezierCurveTo ctx cx1 cy1 cx2 cy2 x2 y2)))

(extend-protocol DirectExecutor
  Object
  (draw! [this ctx]
    (if (lang/template? this)
      (draw! (lang/expand-template this) ctx)
      (println "I don't know how to draw a" (type this))))

  AffineWrapper
  (draw! [this ^GraphicsContext ctx]
    (let [current-atx (.getTransform ctx nil)
          current-width (.getLineWidth ctx)
          [a b c d x y] (lang/aw-matrix this)
          mag (util/magnitude a b c d)]
      (.transform ctx a c b d x y)
      (.setLineWidth ctx (/ current-width mag))
      (draw! (.-shape this) ctx)
      (.setTransform ctx current-atx)
      (.setLineWidth ctx current-width)))

  Style
  (draw! [{:keys [shape style]} ^GraphicsContext ctx]
    (let [old-stroke (.getStroke ctx)]
      (when (and (:stroke style) (not= :none (:stroke style)))
        (.setStroke ctx (:stroke style)))
      (binding [*state* (merge *state* style)]
        (draw! shape ctx))
      (.setStroke ctx old-stroke)))

  Bezier
  (draw! [this ^GraphicsContext ctx]
    (when (stroke?)
      (.beginPath ctx)
      (draw-path! this ctx)
      (.stroke ctx)))

  Spline
  (draw! [{:keys [segments]} ^GraphicsContext ctx]
    (.beginPath ctx)
    (run! #(draw-path! % ctx) segments)
    (when (stroke?)
      (.stroke ctx)))

  Line
  (draw! [{[x1 y1] :from [x2 y2] :to} ^GraphicsContext ctx]
    (.strokeLine ctx x1 y1 x2 y2))

  Circle
  (draw! [{[x y] :centre r :radius} ^GraphicsContext ctx]
    (when (fill?)
      (.fillOval ctx x y r r))
    (when (stroke?)
      (.strokeOval ctx x y r r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; And go!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def init-stack
  (list {:fill      nil
         :stroke    nil
         :opacity   nil
         :font      nil
         :transform [1 0 0 1 0 0]}))

(defn clear! [^GraphicsContext ctx]
  (let [canvas (.getCanvas ctx)]
    (.clearRect ctx 0 0 (.getWidth canvas) (.getHeight canvas))))

(defn simple-render [shape ^GraphicsContext ctx]
  (clear! ctx)
  (draw! shape ctx))
