(ns falloleen.hosts.jfx
  (:require [falloleen.hosts.jfx.impl :as impl])
  (:import falloleen.hosts.jfx.classes.GRoot
           [javafx.application Application Platform]
           [javafx.scene.canvas Canvas GraphicsContext]))

;;;;; Application object API indirection

(defn ^falloleen.hosts.jfx.classes.GRoot instance []
  (when (impl/initialised?)
    (impl/instance)))

(defn width []
  (.getWidth ^Canvas (:canvas @(.-state (instance)))))

(defn height []
  (.getHeight ^Canvas (:canvas @(.-state (instance)))))

(defn ^GraphicsContext ctx []
  (:gc @(.-state (instance))))

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
