(ns falloleen.hosts
  #?
  (:clj (:require
         [falloleen.hosts.jfx :as jfx]
         [falloleen.lang :as lang]
         [falloleen.renderer.fx-canvas :as renderer])
   :cljs (:require
          [goog.object :as obj]
          [falloleen.hosts.browser-canvas :as browser]
          [falloleen.lang :as lang]
          [falloleen.renderer.html-canvas :as renderer])))

(defn default-host [opts]
  #?(:cljs
     (let [state (atom nil)]
       (reify lang/Host
         (width [_]
           (let [elem (:canvas @state)]
             (obj/get elem "width")))
         (height [_]
           (let [elem (:canvas @state)]
             (obj/get elem "height")))
         (initialise [_]
           (when-not @state
             (reset! state (browser/init-canvas opts))))
         (render [_ shape]
           (println @state)
           (let [ctx (:ctx @state)]
             (renderer/simple-render shape ctx)))))
     :clj
     (reify lang/Host
       (width [_] (jfx/width))
       (height [_] (jfx/height))
       (initialise [_]
         (when-not (jfx/instance)
           (jfx/start-fx!)))
       (shutdown [_]
         (jfx/kill-fx!))

       (render [this shape]
         (renderer/simple-render shape (jfx/ctx))))))
