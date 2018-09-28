(ns falloleen.hosts.browser-canvas
  "Hosts Lemonade in an HTML Canvas element in a browser. Assumes a markup
  structure as follows:

  <div id='canvas-container'>
    <canvas id='canvas'></canvas>
  </div>"
  (:require [goog.object :as obj]))

(defn get-elem [id]
  (js/document.getElementById  id))

(defn render-context [elem]
  (.getContext elem "2d"))

(defn element-dimensions [id]
  (let [elem (get-elem id)]
    [(obj/get elem "clientWidth")
     (obj/get elem "clientHeight")]))

(defn set-canvas-size! [canvas [width height]]
  (obj/set canvas "width" width)
  (obj/set canvas "height" height))

(defn window-dimensions []
  [(obj/get js/window "innerWidth")
   (obj/get js/window "innerHeight")])

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

(def fullstyles
  (str "overflow:hidden;"
       "width:100%;"
       "height:100%;"
       "margin:0"))

(defn init-canvas
  "Clears children of the main element and adds a canvas beneath it."
  [opts]
  (let [id   (get opts :id "app")
        elem (get-elem id)
        c    (js/document.createElement "canvas")]
    (unchecked-set elem "innerHTML" "")
    (.appendChild elem c)
    (let [ctx (.getContext c "2d")]
      (when-let [s (:size opts)]
        (cond
          (= s :fullscreen)
          (do
            (run! (fn [e] (.setAttribute e "style" fullstyles))
                  [(aget (js/document.getElementsByTagName "html") 0)
                   js/document.body
                   elem])
            (set-canvas-size! c (element-dimensions id)))

          :fill-container (set-canvas-size! c (element-dimensions id))
          :else           (set-canvas-size! c s)))
      {:canvas c :ctx ctx :elem elem})))
