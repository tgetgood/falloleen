(ns falloleen.renderer.canvas-compiler
  #?@
   (:clj
    [(:require
      [falloleen.lang :as lang]
      [falloleen.math :as math]
      [falloleen.util])
     (:import
      [falloleen.lang
       AffineWrapper
       Arc
       Bezier
       Circle
       ClosedSpline
       Line
       RawText
       Spline
       Style])]
    :cljs
    [(:require
      [falloleen.lang
       :as
       lang
       :refer
       [AffineWrapper
        Arc
        Bezier
        Circle
        ClosedSpline
        Line
        RawText
        Spline
        Style]]
      [falloleen.math :as math]
      [falloleen.util :include-macros true])]))

(defn convert-styles [{:keys [stroke fill opacity font]}]
  (into []
        (remove nil? [(when stroke
                        [:stroke (if (= :none stroke) :none (name stroke))])
                      (when fill
                        [:fill (if (= :none fill) :none (name fill))])
                      (when opacity [:opacity opacity])
                      (when font [:font font])])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Semi-abstract language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Canvas2DRenderable
  (cmds [this] "Returns a map of cmds to draw in various circumstances."))

(falloleen.util/implement-sequentials
 Canvas2DRenderable
 (cmds [this]
   (mapcat cmds this)))

(extend-protocol Canvas2DRenderable
  nil
  (cmds [_]
    (println "Can't render nil. Doing nothing."))

  #?(:cljs default :clj Object)
  (cmds [this]
    (if (lang/template? this)
      (cmds (lang/expand-template this))
      (println "Don't know how to render a "
               (type this) ". Doing nothing.")))

  AffineWrapper
  (cmds [this]
    (lang/compile this
                  (fn [^AffineWrapper this]
                    (let [[a b c d x y] (lang/aw-matrix this)
                          shape (.-shape this)]
                      (conj
                       (into
                        [:save
                         [:transform a b c d x y]]
                        (cmds shape))
                       :restore)))))

  RawText
  (cmds [{:keys [text]}]
    [[:fill-text text 0 0]])

  Line
  (cmds [{[x1 y1] :from [x2 y2] :to}]
    [:begin-path
     [:move-to x1 y1]
     [:line-to x2 y2]
     :end-curve])

  Arc
  (cmds [{r :radius [x y] :centre :keys [from to clockwise?] :as this}]
    (let [[x1 y1] (first (lang/boundary this))]
      [:begin-path
       [:move-to x1 y1]
       [:arc x y r from to (boolean clockwise?)]
       :end-curve]))

  Bezier
  (cmds [{[x1 y1] :from [x2 y2] :to [cx1 cy1] :c1 [cx2 cy2] :c2}]
    [:begin-path
     [:move-to x1 y1]
     [:bezier-curve-to cx1 cy1 cx2 cy2 x2 y2]
     :end-curve])

  Circle
  (cmds [{[x y] :centre r :radius}]
    [:begin-path
     [:arc x y r 0 (* 2 math/pi) true]
     :end-figure])

  Spline
  (cmds [{:keys [segments]}]
    (let [[x y] (first (lang/boundary (first segments)))]
      (conj (transduce (comp (map cmds)
                             cat
                             (remove #(contains? #{:begin-path :end-curve} %))
                             (remove #(= (first %) :move-to)))
                       conj
                       [:begin-path
                        [:move-to x y]]
                       segments)
            :end-curve)))

  ClosedSpline
  (cmds [{:keys [segments]}]
    (let [[x y] (first (lang/boundary (first segments)))]
      (conj (transduce (comp (map cmds)
                             cat
                             (remove #(contains? #{:begin-path :end-curve} %))
                             (remove #(= (first %) :move-to)))
                       conj
                       [:begin-path
                        [:move-to x y]]
                       segments)
            :end-figure)))

  Style
  (cmds [{:keys [style shape]}]

    (into [:save]
          (concat
           (convert-styles style)
           (cmds shape)
           [:restore]))))
