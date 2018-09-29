(ns falloleen.hosts.jfx.classes
  (:require [falloleen.hosts.jfx.impl :as impl])
  (:import javafx.application.Application
           [javafx.scene Group Scene]
           javafx.scene.canvas.Canvas
           javafx.stage.Stage))

(gen-class :name falloleen.hosts.jfx.classes.GRoot
           :extends javafx.application.Application
           :main false
           :post-init intern-instance)

(defn -intern-instance [this & args]
  (deliver impl/instance this))

(defn ^:private -start [^falloleen.hosts.jfx.classes.GRoot this ^Stage s])
