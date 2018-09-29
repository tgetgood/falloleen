(ns falloleen.hosts.jfx.impl
  (:require [clojure.tools.namespace.repl :as nsr]))

(nsr/disable-unload!)
(nsr/disable-reload!)

(defonce instance (promise))
