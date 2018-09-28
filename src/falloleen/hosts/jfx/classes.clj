(ns falloleen.hosts.jfx.classes
  (:require [falloleen.hosts.jfx.impl :as impl])
  (:import javafx.animation.AnimationTimer
           javafx.application.Application
           [javafx.scene Group Scene]
           javafx.scene.canvas.Canvas
           javafx.stage.Stage))

(gen-class :name falloleen.hosts.jfx.classes.Timer
           :extends javafx.animation.AnimationTimer
           :prefix "timer-")

(defn timer-handle [this time]
  )

(gen-class :name falloleen.hosts.jfx.classes.GRoot
           :extends javafx.application.Application
           :main false
           :state state
           :init internal-init
           :post-init intern-instance)

(defn -internal-init []
  [[] (atom nil)])

(defn -intern-instance [this & args]
  (impl/set-instance! this))

(defn ^:private -start [^falloleen.hosts.jfx.classes.GRoot this ^Stage s]
  (let [root   (Group.)
        canvas (Canvas. 500 500)
        gc     (.getGraphicsContext2D canvas)]
    (reset! (.-state this) {:root   root
                            :canvas canvas
                            :gc     gc
                            :stage  s})
    (.. root getChildren (add canvas))
    (doto s
      (.setScene (Scene. root))
      .show)))
