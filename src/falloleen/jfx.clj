(ns falloleen.jfx
  (:require [falloleen.hosts.jfx :as host])
  (:import javafx.application.Platform
           javafx.scene.control.TextArea
           javafx.scene.Scene
           javafx.stage.Stage))

(defonce started
  (try
    (host/start-fx!)
    (catch Exception e ::system-reload)))

(defonce code-stages (atom []))

(defmacro fx-thread [& body]
  `(let [p# (promise)]
    (Platform/runLater (proxy [Runnable] []
                          (run []
                            (let [res# (do ~@body)]
                              (deliver p# res#)))))
    p#))

(defn code-stage []
  (fx-thread
   (let [s (Stage.)
         t (TextArea.)]
     (.setBorder t nil)
     (doto s
       (.setScene (Scene. t))
       .show)
     (swap! code-stages conj s)
     {:stage s :area t})))

(defn clear-stages! []
  (run! #(fx-thread (.close ^Stage %)) @code-stages)
  (reset! code-stages []))
