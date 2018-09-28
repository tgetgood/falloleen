(ns falloleen.hosts.jfx
  (:import javafx.animation.AnimationTimer
           [javafx.application Application Platform]
           [javafx.scene Group Scene]
           [javafx.scene.canvas Canvas GraphicsContext]
           javafx.stage.Stage))

(defonce ^:private instance
  (atom nil))

(defonce ^:private state
  (atom nil))

(defn uninitialised? []
  (nil? @instance))

(defn width []
  (.getWidth ^Canvas (:canvas @state)))

(defn height []
  (.getHeight ^Canvas (:canvas @state)))

(defn ctx []
  (:gc @state))

;;;;; Animation Timer

(gen-class :name falloleen.hosts.jfx.Timer
           :extends javafx.animation.AnimationTimer
           :prefix "timer-")

(defn timer-handle [this time]
  )

;;;;; Main Application

(gen-class :name falloleen.hosts.jfx.Application
           :extends javafx.application.Application
           :main false
           :post-init intern-instance)

(defn -intern-instance [this & args]
  (if @instance
    (println "Instance already set. Don't reset it.")
    (reset! instance this)))

(defn ^:private -start [^falloleen.hosts.jfx.Application this ^Stage s]
  (let [root   (Group.)
        canvas (Canvas. 500 500)
        gc     (.getGraphicsContext2D canvas)]
    (reset! state {:root   root
                   :canvas canvas
                   :gc     gc
                   :stage  s})
    (.. root getChildren (add canvas))
    (doto s
      (.setScene (Scene. root))
      .show)))

;; Invocation

(defonce ^Thread render-thread
  (Thread. (fn []
             (Application/launch falloleen.hosts.jfx.Application
                                 (make-array String 0)))))

(defn start-fx! []
  (Platform/setImplicitExit false)
  (.start render-thread))

(defn kill-fx! []
  (Platform/exit))
