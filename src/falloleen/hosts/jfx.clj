(ns falloleen.hosts.jfx
  (:require [falloleen.hosts.jfx.impl :as impl]
            [falloleen.lang :as lang]
            [falloleen.renderer.fx-canvas :as renderer])
  (:import falloleen.hosts.jfx.classes.GRoot
           [javafx.application Application Platform]
           [javafx.scene.canvas Canvas GraphicsContext]
           javafx.scene.layout.StackPane
           javafx.scene.Scene
           javafx.stage.Stage))

(defonce ^Thread render-thread
  (Thread. (fn []
             (Application/launch falloleen.hosts.jfx.classes.GRoot
                                 (make-array String 0)))))

(defn start-fx! []
  (Platform/setImplicitExit false)
  (.start render-thread))

(defn kill-fx! []
  (Platform/exit))

(defrecord JFXHost [^Canvas canvas ^GraphicsContext ctx]
  ;; N.B.: If I want to programmatically kill the window I need to keep the
  ;; stage around.
  lang/Host
  (dimensions [_] [(.getWidth canvas) (.getHeight canvas)])
  (render [_ shape] (renderer/simple-render shape ctx)))

(defn resizable-canvas [w h]
  (proxy [javafx.scene.canvas.Canvas] [w h]
    (isResizable [] true)))

(defn make-host [opts]
  (when-not (realized? impl/instance)
    (start-fx!))
  @impl/instance
  (let [ugh (promise)]
    (Platform/runLater
     (proxy [Runnable] []
       (run []
         (let [[w h]  (get opts :size [640 480])
               stage  (Stage.)
               root   (StackPane.)
               canvas (resizable-canvas w h)]
           (.setResizable stage true)
           (.. root getChildren (add canvas))
           (.. canvas widthProperty (bind (.widthProperty root)))
           (.. canvas heightProperty (bind (.heightProperty root)))
           (doto stage
             (.setScene (Scene. root))
             .show)
           (deliver ugh canvas)))))
    (let [^Canvas canvas @ugh
          ctx            (.getGraphicsContext2D canvas)]
      (JFXHost. canvas ctx))))
