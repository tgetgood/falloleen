(ns falloleen.hosts
  #?@
   (:clj
    [(:require [falloleen.hosts.jfx :as jfx])]
    :cljs
    [(:require [falloleen.hosts.browser-canvas :as browser])]))

(defn default-host [opts]
  #?(:cljs (browser/make-host opts)
     :clj (jfx/make-host opts)))
