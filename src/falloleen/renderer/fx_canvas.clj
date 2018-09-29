(ns falloleen.renderer.fx-canvas
  (:require [falloleen.math :as math]
            [falloleen.renderer.canvas-compiler :as compiler])
  (:import javafx.scene.canvas.GraphicsContext
           javafx.scene.shape.ArcType))

(defonce tester (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Inefficient and easy method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol DirectExecutor
  "Shapes implementing this protocol can directly draw themselves to screen in
  recursive decent style."
  (draw! [this ctx stack]))

(extend-protocol DirectExecutor
  Object
  (draw! [this ctx stack]
    (println "I don't know how to draw a " (type this))))



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
