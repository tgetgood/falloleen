(ns falloleen.renderer.jsc
  (:require [falloleen.lang :as lang :refer [Bezier Line AffineWrapper Style Spline]]
            [falloleen.math :as math]
            [falloleen.util :include-macros true]))

(defn colour [c]
  (if (= c :none)
    "rgba(0,0,0,0)"
    (if (keyword? c)
      (name c)
      c)))

(defn c-style [{:keys [font stroke fill opacity]} ctx]
  (when-not (nil? font)
    (js* "~{}.font=~{}" ctx font))
  (when-not (nil? opacity)
    (js* "~{}.globalAlpha=~{}" ctx opacity))
  (when-not (nil? stroke)
    (js* "~{}.strokeStyle=~{}" ctx (colour stroke)))
  (when-not (nil? fill)
    (js* "~{}.fillStyle=~{}" ctx (colour fill))))

(defprotocol JSPathCompile
  (path-compile [this ctx]))

(defprotocol JSCompile
  (jsc [this ctx]))

(extend-protocol JSPathCompile
  Line
  (path-compile [{[x2 y2] :to} ctx]
    (js* "~{}.lineTo(~{},~{});" ctx x2 y2))

  Bezier
  (path-compile [{[cx1 cy1] :c1 [cx2 cy2] :c2 [x2 y2] :to} ctx]
    (js* "~{}.bezierCurveTo(~{},~{},~{},~{},~{},~{});"
         ctx cx1 cy1 cx2 cy2 x2 y2)))

(falloleen.util/implement-sequentials
 JSCompile
 (jsc [this ctx]
   (run! #(jsc % ctx) this)))

(extend-protocol JSCompile
  default
  (jsc [this ctx]
    (if (lang/template? this)
      (jsc (lang/expand-template this) ctx)
      (println "Don't know how to render a" (type this))))

  AffineWrapper
  (jsc [this ctx]
    (let [[a b c d x y] (lang/aw-matrix this)
          mag (falloleen.util/magnitude a b c d)]
      (js* "var ctx=~{};ctx.save();ctx.transform(~{},~{},~{},~{},~{},~{});ctx.lineWidth=ctx.lineWidth/~{};"
           ctx a c b d x y mag)
      (jsc (lang/contents this) ctx)
      (js* "~{}.restore();" ctx)))

  Line
  (jsc [{[x1 y1] :from :as this} ctx]
    (js* "~{}.moveTo(~{},~{});" ctx x1 y1)
    (path-compile this ctx)
    (js* "~{}.stroke();" ctx))

  Bezier
  (jsc [{[x1 y1] :from :as this} ctx]
    (js* "~{}.moveTo(~{},~{});" ctx x1 y1)
    (path-compile this ctx)
    (js* "~{}.stroke();" ctx))

  Spline
  (jsc [{:keys [segments]} ctx]
    (let [[x y] (first (lang/boundary (first segments)))]
      (js* "var ctx=~{};ctx.beginPath();ctx.moveTo(~{},~{});"
           ctx x y)
      (run! #(path-compile % ctx) segments)
      (js* "~{}.stroke();" ctx)))

  Style
  (jsc [{:keys [style shape]} ctx]
    (js* "~{}.save();" ctx)
    (c-style style ctx)
    (jsc shape ctx)
    (js* "~{}.restore();" ctx)))

(defn- clear-screen!
  "Clear the rendering context's canvas."
  [ctx]
  (.clearRect ctx 0 0 (-> ctx .-canvas .-width) (-> ctx .-canvas .-height)))

(defn render! [shape ctx]
  (clear-screen! ctx)
  (js* "~{}.fillStyle='rgba(0,0,0,0)';" ctx)
  (jsc shape ctx))
