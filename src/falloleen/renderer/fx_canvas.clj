(ns falloleen.renderer.fx-canvas
  (:require [falloleen.lang :as lang]
            [falloleen.util :as util])
  (:import [falloleen.lang AffineWrapper Circle Line]
           javafx.scene.canvas.GraphicsContext))

(defn in-path? [stack]
  (:path stack))

(defn fill? [stack]
  (when-let [f (:fill stack)]
    (not= :none f)))

(defn stroke? [stack]
  (when-let [s (:stroke stack)]
    (not= ::none s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Inefficient and easy method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol DirectExecutor
  "Shapes implementing this protocol can directly draw themselves to screen in
  recursive decent style."
  (draw! [this ctx stack]))

(util/implement-sequentials
    DirectExecutor
  (draw! [this ctx stack]
    (run! #(draw! % ctx stack) this)))

(extend-protocol DirectExecutor
  Object
  (draw! [this ctx stack]
    (if (lang/template? this)
      (draw! (lang/expand-template this) ctx stack)
      (println "I don't know how to draw a" (type this))))

  AffineWrapper
  (draw! [this ^GraphicsContext ctx stack]
    (let [current-atx (.getTransform ctx nil)
          current-width (.getLineWidth ctx)
          [a b c d x y] (lang/aw-matrix this)
          mag (util/magnitude a b c d)]
      (.transform ctx a c b d x y)
      (.setLineWidth ctx (/ current-width mag))
      (draw! (.-shape this) ctx stack)
      (.setTransform ctx current-atx)
      (.setLineWidth ctx current-width)))

  ;; Bezier

  Line
  (draw! [{[x1 y1] :from [x2 y2] :to} ^GraphicsContext ctx stack]
    (.strokeLine ctx x1 y1 x2 y2))

  Circle
  (draw! [{[x y] :centre r :radius} ^GraphicsContext ctx stack]
    (when (fill? stack)
      (.fillOval ctx x y r r))
    (.strokeOval ctx x y r r)))



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
  (draw! shape ctx init-stack))
