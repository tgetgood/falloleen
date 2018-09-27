(ns falloleen.demo
  (:require [falloleen.core :as core]
            [falloleen.hosts :as hosts]
            [falloleen.lang :as lang]))

(defonce host (atom nil))

(defn start-once! []
  (reset! host (hosts/default-host {:size :fullscreen}))
  (lang/initialise @host))

(defn go []
  (core/draw! [(assoc core/circle :radius 200)] @host))
