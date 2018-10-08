(ns falloleen.renderer.canvas-compiler
   (:require-macros [falloleen.renderer.canvas-compiler :refer [instructions]])
   (:require [falloleen.lang :as lang]
             [falloleen.math :as math]
             [falloleen.util :include-macros true]))

(defrecord Fill [colour])
(defrecord Stroke [colour])
(defrecord Font [font])
(defrecord Opacity [opacity])

(defrecord Save [])
(def save (Save.))

(defrecord Restore [])
(def restore (Restore.))

(defrecord BeginPath [])
(def begin-path (BeginPath.))

(defrecord EndPath [])
(def end-path (EndPath.))

(defrecord EndFigure [])
(def end-figure (EndFigure.))

(defrecord Transform [matrix])
(defn transform [m]
  (Transform. m))

(defrecord MoveTo [x y])
(defn move-to [x y]
  (MoveTo. x y))

(defrecord FillText [text])
(defn fill-text [text]
  (FillText. text))

(defrecord LineTo [x y])
(defn line-to [x y]
  (LineTo. x y))

(defrecord BezierCurveTo [cx1 cy1 cx2 cy2 x2 y2])
(defn bezier-curve-to [cx1 cy1 cx2 cy2 x2 y2]
  (BezierCurveTo. cx1 cy1 cx2 cy2 x2 y2))

(defrecord Arc [x y r from to clockwise?])
(defn arc [x y r from to clock?]
  (Arc. x y r from to clock?))

(defrecord InstructionSet [cmds])

(defn instructions* [args]
  (InstructionSet. args))

(defn convert-styles [{:keys [stroke fill opacity font]}]
  (instructions*
         (remove nil? [(when stroke
                         (Stroke. (if (= :none stroke) :none (name stroke))))
                       (when fill
                         (Fill. (if (= :none fill) :none (name fill))))
                       (when opacity (Opacity. opacity))
                       (when font (Font. font))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Semi-abstract language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce compile-cache (atom {}))

(defprotocol PathRenderable
  (path-cmds [this]))

(extend-protocol PathRenderable
  lang/Line
  (path-cmds [{[x2 y2] :to}]
    (instructions
     (line-to x2 y2)))

  lang/Arc
  (path-cmds [{[x y] :centre r :radius :keys [to from clockwise?]}]
    (instructions
     (arc x y r from to clockwise?)))

  lang/Bezier
  (path-cmds [{[cx1 cy1] :c1 [cx2 cy2] :c2 [x2 y2] :to}]
    (instructions
     (bezier-curve-to cx1 cy1 cx2 cy2 x2 y2))))

(defprotocol Canvas2DRenderable
  (cmds [this] "Returns a map of cmds to draw in various circumstances."))

(falloleen.util/implement-sequentials
 Canvas2DRenderable
 (cmds [this]
   (instructions* (map cmds this))))

(extend-protocol Canvas2DRenderable
  nil
  (cmds [_]
    (println "Can't render nil. Doing nothing."))

  default
  (cmds [this]
    (if (lang/template? this)
      (cmds (lang/expand-template this))
      (println "Don't know how to render a "
               (type this) ". Doing nothing.")))

  lang/AffineWrapper
  (cmds [this]
    (if-let [code (lang/getc this ::code)]
      code
      (let [code (instructions
                  save
                  (transform (lang/aw-matrix this))
                  (cmds (.-shape this))
                  restore)]
        (lang/setc this ::code code)
        code)))

  lang/RawText
  (cmds [{:keys [text]}]
    (instructions
     (fill-text text)))

  lang/Line
  (cmds [{[x1 y1] :from :as this}]
    (instructions
     begin-path
     (move-to x1 y1)
     (path-cmds this)
     end-path))

  lang/Arc
  (cmds [this]
    (let [[x1 y1] (first (lang/boundary this))]
      (instructions
       begin-path
       (move-to x1 y1)
       (path-cmds this)
       end-path)))

  lang/Bezier
  (cmds [{[x1 y1] :from :as this}]
    (instructions
     begin-path
     (move-to x1 y1)
     (path-cmds this)
     end-path))

  lang/Circle
  (cmds [{[x y] :centre r :radius}]
    (instructions
     begin-path
     (arc x y r 0 (* 2 math/pi) true)
     end-path))

  lang/Spline
  (cmds [{:keys [segments]}]
    (let [[x y] (first (lang/boundary (first segments)))]
      (instructions
       begin-path
       (move-to x y)
       (instructions* (map path-cmds segments))
       end-path)))

  lang/ClosedSpline
  (cmds [{:keys [segments]}]
    (let [[x y] (first (lang/boundary (first segments)))]
      (instructions
       begin-path
       (move-to x y)
       (instructions* (map path-cmds segments))
       end-figure)))

  lang/Style
  (cmds [{:keys [style shape]}]
    (instructions
     save
     (convert-styles style)
     (cmds shape)
     restore)))
