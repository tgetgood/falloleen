(ns falloleen.hosts
  #?
  (:clj (:require
         [falloleen.lang :as lang])
   :cljs (:require
          [goog.object :as obj]
          [falloleen.hosts.browser-canvas :as browser]
          [falloleen.lang :as lang])))

(defn default-host [opts]
  #?(:cljs
     (let [id (get opts :id "canvas")
           elem (browser/canvas-elem id)]
       (when-let [s (:size opts)]
         (if (= s :fullscreen)
           (browser/fill-container! id)
           (browser/set-canvas-size! elem s)))
       (reify lang/Host
         (base [_] elem)
         (width [_] (obj/get elem "width"))
         (height [_] (obj/get elem "height"))
         (render [_ shape] (constantly nil))))))
