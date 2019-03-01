(ns falloleen.hosts.jfx
  (:require [falloleen.hosts.jfx.impl :as impl]
            [falloleen.lang :as lang]
            [falloleen.renderer.fx-canvas :as renderer])
  (:import falloleen.hosts.jfx.classes.GRoot
           [javafx.application Application Platform]
           [javafx.scene.canvas Canvas GraphicsContext]
           javafx.scene.Group
           javafx.scene.Scene
           javafx.stage.Stage))

(defonce ^Thread render-thread
  (Thread. (fn []
             (Application/launch falloleen.hosts.jfx.classes.GRoot
                                 (make-array String 0)))))

(defn start-fx! []
  (Platform/setImplicitExit false)
  (.start render-thread))

(defonce started
  (try
    (start-fx!)
    (catch Exception e ::system-reload)))

(defn kill-fx! []
  (Platform/exit))

(defrecord JFXHost [^Stage stage ^Canvas canvas ^GraphicsContext ctx]
  ;; N.B.: If I want to programmatically kill the window I need to keep the
  ;; stage around.
  lang/Host
  (close! [_] (.close stage))
  (dimensions [_] [(.getWidth canvas) (.getHeight canvas)])
  (render [this shape] (renderer/simple-render shape this)))

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
               root   (Group.)
               canvas (resizable-canvas w h)]
           (.setResizable stage true)
           (.. root getChildren (add canvas))
           ;; (.. canvas widthProperty (bind (.widthProperty stage)))
           ;; (.. canvas heightProperty (bind (.heightProperty stage)))
           (doto stage
             (.setScene (Scene. root))
             .show)
           (deliver ugh [stage canvas])))))
    (let [[stage canvas] @ugh
          ctx            (.getGraphicsContext2D ^Canvas canvas)]
      (JFXHost. stage canvas ctx))))
