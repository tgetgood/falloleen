(ns falloleen.hosts.jfx.impl
  (:require [clojure.tools.namespace.repl :as nsr]))

(nsr/disable-unload!)
(nsr/disable-reload!)

(defonce ^:private the-instance
  (atom nil))

(defn set-instance! [i]
  (if (nil? @the-instance)
    (reset! the-instance i)
    (throw (Exception. (str "Don't ever overwrite the fx"
                            " application intance. Damn singletons.")))))

(defn instance []
  @the-instance)

(defn initialised? []
  (not (nil? @the-instance)))
