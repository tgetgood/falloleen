(ns falloleen.hosts.browser-canvas
  "Hosts Lemonade in an HTML Canvas element in a browser. Assumes a markup
  structure as follows:

  <div id='canvas-container'>
    <canvas id='canvas'></canvas>
  </div>"
  (:require [goog.object :as obj]))

(defn canvas-elem [id]
  (js/document.getElementById  id))

(defn canvas-container [id]
  (let [container-id (str id "-container")]
    (js/document.getElementById container-id)))

(defn canvas-container-dimensions [id]
  (let [cc (canvas-container id)]
    [(obj/get cc "clientWidth") (obj/get cc "clientHeight")]))

(defn set-canvas-size! [canvas [width height]]
  (obj/set canvas "width" width)
  (obj/set canvas "height" height))

(defn fill-container! [id]
  (let [[w h :as dim] (canvas-container-dimensions id)]
    (set-canvas-size! (canvas-elem id) dim)))

(defn watch-resize [cb]
  (let [running (atom false)]
    (set! (.-onresize js/window)
          (fn []
            (when (compare-and-set! running false true)
              (js/setTimeout
               (fn []
                 (when (compare-and-set! running true false)
                   (cb)))
               200))))))
