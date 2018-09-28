(ns falloleen.renderer.fx-canvas
  (:require [falloleen.renderer.canvas-compiler :as compiler])
  (:import [javafx.scene.canvas Canvas GraphicsContext]))

(defn clear! [^GraphicsContext ctx]
  (let [canvas (.getCanvas ctx)]
    (.clearRect ctx 0 0 (.getWidth canvas) (.getHeight canvas))))

(defonce tester (atom nil))

(defn simple-render [shape ctx]
  (reset! tester (compiler/cmds shape)))
