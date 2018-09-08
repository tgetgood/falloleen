(ns falloleen.renderer.canvas
  (:require [falloleen.lang :as lang])
  (:require-macros [falloleen.renderer.canvas :refer [defcanvas]]
                   [falloleen.renderer.util :as util]))

(defprotocol IExec
  (exec [this ctx] "Execute this instruction on ctx."))

(defprotocol Canvas2DRenderable
  (cmds [this] "Returns a map of cmds to draw in various circumstances."))

(defcanvas save)
(defcanvas restore)
(defcanvas stroke)
(defcanvas fill)
(defcanvas begin-path)
(defcanvas close-path)
(defcanvas clip)

(defcanvas fill-style [s])
(defcanvas stroke-style [s])
(defcanvas global-alpha [a])
(defcanvas font [f])


(defcanvas move-to [x y])
(defcanvas line-to [x y])
(defcanvas arc [x y r from to clockwise?])
(defcanvas bezier-curve-to [cx1 cy1 cx2 cy2 x2 y2])
(defcanvas rect [x y width height])

(defcanvas fill-text [text x y])

(defcanvas transform [a b c d x y])

(defcanvas line-width-hack [mag])

(util/implement-sequentials
 Canvas2DRenderable
 (cmds [this]
   {:draw (into [] (map cmds) this)}))

(extend-protocol Canvas2DRenderable
  nil
  (cmds [_]
    (println "Can't render nil. Doing nothing."))

  default
  (cmds [this]
    (let [t (type this)]
      (if (satisfies? lang/ITemplate this)
        (cmds (lang/expand-template this))
        (println "Don't know how to render a "
                 t ". Doing nothing."))))

  lang/Transformed
  (cmds [this]
    {:draw (cmds (.-base this))})

  lang/RawText
  (cmds [{[x y] :corner :keys [text]}]
)

  ;; Templates can implement protocols too
  ;; lang/Rectangle
  ;; (cmds [{:keys [style width height] [x y] :corner}]
  ;;   (compile-leaf {:style style
  ;;                  :pre   [*begin-path]
  ;;                  :draw  [(call "rect" x y width height)]
  ;;                  :post  [*maybe-fill
  ;;                          *stroke]}))


  lang/Line
  (cmds [{[x1 y1] :from [x2 y2] :to}]
    {:pre   [begin-path
             (move-to x1 y1)]
     :draw  [(line-to x2 y2)]
     :post  [stroke]})

  lang/Arc
  (cmds [{r :radius [x y] :centre :keys [from to clockwise?] :as this}]
    (let [[x1 y1] (first (lang/endpoints this))]
      {:pre   [begin-path
               (move-to x1 y1)]
       :draw  [(arc x y r from to (boolean clockwise?))]
       :post  [stroke]}))

  lang/Bezier
  (cmds [{[x1 y1] :from [x2 y2] :to [cx1 cy1] :c1 [cx2 cy2] :c2}]
    {:pre  [begin-path
            (move-to x1 y1)]
     :draw [(bezier-curve-to cx1 cy1 cx2 cy2 x2 y2)]
     :post [stroke]})
)
