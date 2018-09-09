(ns falloleen.renderer.canvas
  (:require [falloleen.lang :as lang]
            [falloleen.math :as math])
  (:require-macros [falloleen.renderer.canvas :refer [defcanvas]]
                   [falloleen.renderer.util :as util]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IExec
  (exec [this ctx state] "Execute this instruction on ctx."))

(defprotocol Canvas2DRenderable
  (cmds [this] "Returns a map of cmds to draw in various circumstances."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Styling Stack Machine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (let [v (get (first stack) p)]
    (or (nil? v) (= :default v) )))

(defn assoc-style [stack k v]
  (cons (assoc (first stack) k v) (rest stack)))

(defn update-style [stack k f]
  (let [v (get (first stack) k)
        v (if (= :default k) (get default-styles k))]
    (cons (assoc (first stack) k (f v)) (rest stack))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Canvas Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Stateful Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (run! (fn [[k v]]
              (if (= :transform k)
                (let [[a b c d e f] (if (= :default v) [1 0 0 1 0 0] v)]
                  (.setTransform ctx a c b d e f))
                (if (= :default v)
                  (exec (get default-styles k) ctx nil)
                  (exec v ctx nil))))
            diff)
      (pop-frame stack))))

(def save (Save.))
(def restore (Restore.))

(deftype Transform [a b c d x y]
  IPrintWithWriter
  (-pr-writer [o w _]
   (write-all w (str "#Transformation" o)))

  IExec
  (exec [_ ctx stack]
    (.transform ctx a c b d x y)
    (let [current (get (first stack) :transform)
          current (if (= current :default) [1 0 0 1 0 0] current)]
      (assoc-style stack :transform (math/comp-atx current [a b c d x y])))))

(defn transform [a b c d x y]
  (Transform. a b c d x y))

(deftype LineWidthHack [mag]
    )

(defn line-width-hack [mag]
  (LineWidthHack. mag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Compiling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-protocol IExec
  nil
  (exec [_ _ _]))

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
    (let [[a b c d x y] (lang/atx this)]
      {:pre [save
             (transform a b c d x y)
             (line-width-hack (det a b c d))]
       :draw (cmds (lang/contents this))
       :post [restore]}))

  lang/RawText
  (cmds [{:keys [text]}]
    (fill-text text -100 -100))

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

  lang/Circle
  lang/Spline
  lang/ClosedSpline
  lang/Style
  (cmds [{:keys [style base]}]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Render
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- clear-screen!
  "Clear the rendering context's canvas."
  [ctx]
  (.clearRect ctx 0 0 (-> ctx .-canvas .-width) (-> ctx .-canvas .-height)))

(defn- context
  "Returns the 2d rendering context for the given HTML canvas DOM element."
  [elem]
  (.getContext elem "2d"))

(defn flatten-instructions [x]
  (cond
    (map? x)        (flatten (map flatten-instructions (vals x)))
    (sequential? x) (map flatten-instructions x)
    :else           x))

(defn simple-render [shape ctx]
  (.setTransform ctx 1 0 0 1 0 0)
  (clear-screen! ctx)
  (reduce (fn [acc x] (exec x ctx acc))
          init-stack
          (flatten-instructions (cmds shape))))
