(ns falloleen.hosts.jfx
  (:require [falloleen.hosts.jfx.impl :as impl]
            [falloleen.lang :as lang]
            [falloleen.renderer.fx-canvas :as renderer])
  (:import falloleen.hosts.jfx.classes.GRoot
           [javafx.application Application Platform]
           [javafx.scene.canvas Canvas GraphicsContext]
           javafx.stage.Stage))

;;;;; Application object API indirection

(defn ^falloleen.hosts.jfx.classes.GRoot instance []
  (when (impl/initialised?)
    (impl/instance)))

;;;;; Graphics system

(defonce ^Thread render-thread
  (Thread. (fn []
             (Application/launch falloleen.hosts.jfx.classes.GRoot
                                 (make-array String 0)))))

(defn start-fx! []
  (Platform/setImplicitExit false)
  (.start render-thread))

(defn kill-fx! []
  (Platform/exit))

(defrecord JFXHost [^Stage stage ^Canvas canvas ^GraphicsContext ctx]
  lang/Host
  (dimensions [_] [(.getWidth canvas) (.getHeight canvas)])
  (render [_ shape] (renderer/simple-render shape ctx)))

(defn make-host [opts]
  (when-not (instance)
    (start-fx!))
  (let [{:keys [canvas stage gc]} @(.-state (instance))]
    (JFXHost. stage canvas gc)))
