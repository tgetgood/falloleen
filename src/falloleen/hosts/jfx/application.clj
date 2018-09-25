(ns falloleen.hosts.jfx.application
  (:gen-class :extends javafx.application.Application
               :constructors {[] []})
  (:import [javafx.application Application]
           [javafx.scene Group Scene]
           [javafx.scene.canvas Canvas GraphicsContext]
           javafx.stage.Stage))


(defn -start [this ^Stage s]
  (let [root   (Group.)
        canvas (Canvas. 500 500)
        gc     (.getGraphicsContext2D canvas)]
    #_(draw! gc)
    (.. root getChildren (add canvas))
    (doto s
      (.setScene (Scene. root))
      .show)))
