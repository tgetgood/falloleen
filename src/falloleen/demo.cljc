(ns falloleen.demo
  (:require [falloleen.core :as core]
            [falloleen.hosts :as hosts]
            [falloleen.lang :as lang]))

(defonce host (hosts/default-host {:size :fullscreen}))

(defn start-once! []
  (lang/initialise host))

(defn go []
  (core/draw! [(assoc core/circle :radius 200)] host))
