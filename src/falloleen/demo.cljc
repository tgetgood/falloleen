(ns falloleen.demo
  (:require [falloleen.core :as core]
            [falloleen.hosts :as hosts]))

(defonce host (hosts/default-host {:size [500 500]}))

(defn go []
  (core/draw! [(assoc core/circle :radius 200)] host))
