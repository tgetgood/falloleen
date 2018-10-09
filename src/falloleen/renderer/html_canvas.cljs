(ns falloleen.renderer.html-canvas
  (:require [falloleen.math :as math]
            [falloleen.renderer.canvas-compiler :as compiler]))

(def default-styles
  "This is really just for reference."
  {:fill      "rgba(0,0,0,0)"
   :stroke    "rgba(0,0,0,1)"
   :opacity   1
   :font      "10px sans-serif"
   :transform [1 0 0 1 0 0]})

(defprotocol ExecuteRender
  (exec [this ctx]))

(extend-protocol ExecuteRender
  compiler/InstructionSet
  (exec [this ctx]
    (run! #(exec % ctx) (:cmds this)))

  default
  (exec [i _]
    (println "Unknown instruction " (type i) ", Skipping."))

  compiler/Save
  (exec [_ ctx]
    (.save ctx))

  compiler/Restore
  (exec [_ ctx]
    (.restore ctx))

  compiler/BeginPath
  (exec [_ ctx]
    (.beginPath ctx))

  compiler/EndPath
  (exec [_ ctx]
    (.stroke ctx))

  compiler/EndFigure
  (exec [_ ctx]
    (.stroke ctx)
    (.fill ctx))

  compiler/MoveTo
  (exec [{:keys [x y]} ctx]
    (.moveTo ctx x y))

  compiler/LineTo
  (exec [{:keys [x y]} ctx]
    (.lineTo ctx x y))

  compiler/FillText
  (exec [{:keys [text]} ctx]
    (.fillText ctx text 0 0))

  compiler/BezierCurveTo
  (exec [{:keys [cx1 cy1 cx2 cy2 x2 y2]} ctx]
    (.bezierCurveTo ctx cx1 cy1 cx2 cy2 x2 y2))

  compiler/Arc
  (exec [{:keys [x y r from to clockwise?]} ctx]
    (.arc ctx x y r from to clockwise?))

  compiler/Transform
  (exec [{[a b c d x y] :matrix} ctx]
    (let [mag (falloleen.util/magnitude a b c d)]
      (.transform ctx a c b d x y)
      (unchecked-set ctx "lineWidth" (/ (unchecked-get ctx "lineWidth") mag))))

  compiler/Stroke
  (exec [{m :colour} ctx]
    (let [p (if (= m :none) "rgba(0,0,0,0)" m)]
      (unchecked-set ctx "strokeStyle" p)))

  compiler/Fill
  (exec [{m :colour} ctx]
    (let [p (if (= m :none) "rgba(0,0,0,0)" m)]
      (unchecked-set ctx "fillStyle" p)))

  compiler/Font
  (exec [{m :font} ctx]
    (unchecked-set ctx "font" m))

  compiler/Opacity
  (exec [{m :opacity} ctx]
    (unchecked-set ctx "globalAlpha" m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Render
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- clear-screen!
  "Clear the rendering context's canvas."
  [ctx]
  (.clearRect ctx 0 0 (-> ctx .-canvas .-width) (-> ctx .-canvas .-height)))

(defn simple-render [shape ctx]
  (clear-screen! ctx)
  (exec (compiler/cmds shape) ctx))
