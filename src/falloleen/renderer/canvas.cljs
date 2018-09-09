(ns falloleen.renderer.canvas
  (:require [falloleen.lang :as lang]
            [falloleen.math :as math])
  (:require-macros [falloleen.renderer.canvas :refer [defcanvas]]
                   [falloleen.renderer.util :as util]))

(defprotocol IExec
  (exec [this ctx state] "Execute this instruction on ctx."))

(defprotocol Canvas2DRenderable
  (cmds [this] "Returns a map of cmds to draw in various circumstances."))

(defn push-down [stack]
  (cons (first stack) stack))

(defn pop-frame [stack]
  (rest stack))

(defn diff-top [stack]
  (if (< (count stack) 2)
    (first stack)
    (let [f0 (first stack)
          f1 (second stack)]
      (into {}
            (map (fn [[k v]]
                   (when-not (= v (get f0 k))
                     [k v]))
                 f1)))))

(defn free? [stack p]
  (not (contains? (first stack) p)))

(defn assoc-style [stack k v]
  (cons (assoc (first stack) k v) (rest stack)))

(defn update-style [stack k f]
  (cons (update (first stack) k f) (rest stack)))

(defcanvas stroke)
(defcanvas fill)
(defcanvas begin-path)
(defcanvas close-path)
(defcanvas clip)

(defcanvas move-to [x y])
(defcanvas line-to [x y])
(defcanvas arc [x y r from to clockwise?])
(defcanvas bezier-curve-to [cx1 cy1 cx2 cy2 x2 y2])
(defcanvas rect [x y width height])

(defcanvas fill-text [text x y])



(defcanvas fill-style [s])
(defcanvas stroke-style [s])
(defcanvas global-alpha [a])
(defcanvas font [f])

(def default-styles
  {:fill-style   "rgba(0,0,0,0)"
   :stroke-style "rgba(0,0,0,1)"
   :global-alpha 1
   :font         "sans serif 10px"
   :transform    [1 0 0 1 0 0]})

(def init-stack
  (list {:fill-style   :default
         :stroke-style :default
         :global-alpha :default
         :font         :default
         :transform    :default}))

(deftype Save []
  IPrintWithWriter
  (-pr-writer [_ w _]
   (write-all w (str "#SaveState")))

  IExec
  (exec [_ _ stack]
    (push-down stack)))

(deftype Restore []
  IPrintWithWriter
  (-pr-writer [_ w _]
   (write-all w (str "#RestoreState")))

  IExec
  (exec [_ ctx stack]
    (let [diff (diff-top stack)]
      (run! exec (vals diff))
      (pop-frame stack))))

(def save (Save.))
(def restore (Restore.))

(defcanvas translate [x y])

(deftype Transform [m]
  IPrintWithWriter
  (-pr-writer [o w _]
   (write-all w (str "#Transformation" m)))

  IExec
  (exec [_ ctx stack]
    (let [[a b c d x y] m]
      (.transform ctx a c b d x y)
      (update-style stack :transform #(math/comp-atx % m)))))

;; (defcanvas line-width-hack [mag])

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

  lang/TransformedShape
  (cmds [this]
    {:pre [(Transform. (lang/atx this))
           save]
     :draw (cmds (lang/contents this))
     :post [restore]})

  lang/RawText
  (cmds [{:keys [text]}]
    (fill-text text 0 0))

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
