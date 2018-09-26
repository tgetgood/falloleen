(ns falloleen.hosts
  #?
  (:clj (:require
         [falloleen.hosts.jfx :as jfx]
         [falloleen.lang :as lang]
         [falloleen.renderer.canvas :as renderer])
   :cljs (:require
          [goog.object :as obj]
          [falloleen.hosts.browser-canvas :as browser]
          [falloleen.lang :as lang]
          [falloleen.renderer.canvas :as renderer])))

(defn default-host [opts]
  #?(:cljs
     (let [id (get opts :id "canvas")
           elem (browser/canvas-elem id)
           ctx (browser/render-context elem)]
       (when-let [s (:size opts)]
         (if (= s :fullscreen)
           (browser/fill-container! id)
           (browser/set-canvas-size! elem s)))
       (reify lang/Host
         (width [_] (obj/get elem "width"))
         (height [_] (obj/get elem "height"))
         (render [this shape]
           (renderer/simple-render shape ctx))))
     :clj
     (reify lang/Host
       (width [_] (jfx/width))
       (height [_] (jfx/height))
       (render [this shape]
         (renderer/simple-render shape (jfx/ctx))))))
