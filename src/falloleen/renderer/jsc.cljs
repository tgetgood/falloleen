(ns falloleen.renderer.jsc
  (:require [falloleen.lang :as lang :refer [Bezier Line AffineWrapper Style Spline]]
            [falloleen.math :as math]
            [falloleen.util :include-macros true]))

(defn colour [c]
  (if (= c :none)
    "'rgba(0,0,0,0)'"
    (if (keyword? c)
      (name c)
      c)))

(defn c-style [{:keys [font stroke fill opacity]}]
  (str (when font
         (str "ctx.font=" font ";"))
       (when opacity
         (str "ctx.globalAlpha=" opacity ";"))
       (when stroke
         (str "ctx.strokeStyle=" (colour stroke) ";"))
       (when fill
         (str "ctx.fillStyle=" (colour fill) ";"))))

(defprotocol JSPathCompile
  (path-compile [this]))

(defprotocol JSCompile
  (jsc [this]))

(extend-protocol JSPathCompile
  Line
  (path-compile [{[x2 y2] :to}]
    (str "ctx.lineTo(" x2"," y2 ");"))

  Bezier
  (path-compile [{[cx1 cy1] :c1 [cx2 cy2] :c2 [x2 y2] :to}]
    (str "ctx.bezierCurveTo(" cx1 "," cy1 "," cx2 "," cy2 "," x2 "," y2 ");")))

(falloleen.util/implement-sequentials
 JSCompile
 (jsc [this]
   (transduce (map jsc) str "" this)))

(extend-protocol JSCompile
  default
  (jsc [this]
    (if (lang/template? this)
      (jsc (lang/expand-template this))
      (println "Don't know how to render a" (type this))))

  AffineWrapper
  (jsc [this]
    (if-let [c (lang/getc this ::code)]
      c
      (let [[a b c d x y] (lang/aw-matrix this)
            mag (falloleen.util/magnitude a b c d)
            code (str "ctx.save();"
                      "ctx.transform(" a "," c "," b "," d "," x "," y ");"
                      "ctx.lineWidth=ctx.lineWidth/" mag ";"
                      (jsc (lang/contents this))
                      "ctx.restore();")]
        (lang/setc this ::code code)
        code)))

  Line
  (jsc [{[x1 y1] :from :as this}]
    (str "ctx.moveTo(" x1 "," y1 ");"
         (path-compile this)
         "ctx.stroke();"))

  Bezier
  (jsc [{[x1 y1] :from :as this}]
    (str "ctx.moveTo(" x1 "," y1 ");"
         (path-compile this)
         "ctx.stroke()"))

  Spline
  (jsc [{:keys [segments]}]
    (let [[x y] (first (lang/boundary (first segments)))]
      (str "ctx.beginPath();"
           "ctx.moveTo(" x "," y ");"
           (transduce (map path-compile) str "" segments)
           "ctx.stroke();")))

  Style
  (jsc [{:keys [style shape]}]
    (str "ctx.save();"
         (c-style style)
         (jsc shape)
         "ctx.restore();")))

(defn- clear-screen!
  "Clear the rendering context's canvas."
  [ctx]
  (.clearRect ctx 0 0 (-> ctx .-canvas .-width) (-> ctx .-canvas .-height)))

(defn render! [shape ctx]
  (clear-screen! ctx)
  ((js/eval (str "((ctx) => {"
             "ctx.fillStyle='rgba(0,0,0,0)';"
             (jsc shape) "})")) ctx))
