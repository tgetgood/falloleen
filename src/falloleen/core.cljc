(ns falloleen.core
  (:require [clojure.string :as string]
            [falloleen.hosts :as hosts]
            [falloleen.lang :as lang]
            [falloleen.math :as math]
            [net.cgrand.macrovich :as macros :include-macros true])
  #?(:cljs (:require-macros [falloleen.core :refer [deftemplate]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translate
  [shape v]
  (lang/stack-transform shape (lang/translation v)))

(defn reflect
  ([shape axis]
   (lang/stack-transform shape (lang/reflection axis)))
  ([shape centre axis]
   (lang/transform-with-centre shape centre (lang/reflection axis))))

(defn scale
  ([shape extent]
   (lang/stack-transform shape (lang/scaling extent)))
  ([shape centre extent]
   (lang/transform-with-centre shape centre (lang/scaling extent))))

(defn rotate
  ([shape angle]
   (lang/stack-transform shape (lang/rotation angle)))
  ([shape centre angle]
   (lang/transform-with-centre shape centre (lang/rotation angle))))

(defn transform
  [shape xform]
  (lang/stack-transform shape (lang/build-atx xform)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- type-case
  "Converts idomatic kebab-case to UpperCamelCase for type names."
  [sym]
  (symbol (apply str (map string/capitalize (string/split (name sym) #"-")))))

(macros/deftime
  (defmacro deftemplate
    "Defines a new shape template. Creates a new record whose name is
  instance-name converted to UpperCamelCase as per record naming conventions.

  The canonical instance of the new template will be bound to instance-name. The
  canonical instance is the expansion of the default template arguments.

  Expansion will be executed in an environment when all keys of the template
  name have been bound to symbols. Expansions must return a valid shape
  (template or otherwise).

  Optionally impls are protocol implementations as per defrecord."
    {:style/indent [1 :form :form :form [1]]}
    [instance-name & args]
    (let [docstr (if (string? (first args)) (first args) nil)
          args (if docstr (rest args) args)
          [template expansion & impls] args
          template-name (type-case instance-name)
          fields (map (comp symbol name) (keys template))]
      `(do
         ;; TODO: I can generate a spec from the field list and then check it's
         ;; valid at expansion time. I think that would be a good place to find
         ;; errors.
         ;;
         ;; The problem is that adding a spec/def into this macro expansion
         ;; causes the whole thing to go haywire even though the relevant parts
         ;; of the expansion don't change at all...
         (defrecord ~template-name [~@fields]
           falloleen.lang/ITemplate
           (falloleen.lang/expand-template [this#]
             ~expansion)
           ~@impls)
         (def ~instance-name
           ~@(when docstr [docstr])
           (~(symbol (str "map->" template-name)) ~template))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Built in Shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def line
  "Line segment."
  (lang/map->Line
   {:from [0 0]
    :to   [1 1]}))

(def bezier
  "Bezier cubic."
  (lang/map->Bezier
   {:from [0 0]
    :c1 [0 0]
    :c2 [1 1]
    :to [1 1]}))

(def arc
  "Circular arc."
  (lang/map->Arc
   {:centre [0 0]
    :radius 1
    :from   0
    :to     math/pi
    :clockwise? false}))

(def circle
  "A complete circle."
  (lang/map->Circle
   {:centre [0 0]
    :radius 1}))

(def rectangle
  "Need I say more?"
  (lang/map->Rectangle
   {:origin [0 0]
    :width 1
    :height 1}))

(defn path
  "Takes a sequence of curve segments, each beginning where the previous
  ended. Returns a path consisting of those segments glued together."
  [segs]
  (lang/spline segs))

(defn region
  "Given a closed, connected sequence of curve segments, returns the region
  bounded by that path."
  [segs]
  (lang/closed-spline segs))

(deftemplate polyline
  "Returns the curve created by joining points together with line segments in
  the order given."
  {:points []}
  (let [segs (map (fn [[x y]]
                    (assoc line
                           :from x
                           :to   y))
                   (partition 2 (interleave points (rest points))))]
    (path segs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate polygon
  "Create a polygon from a list of verticies. "
  {:verticies []}
  (let [edges (map (fn [[x y]]
                    (assoc line
                           :from x
                           :to   y))
                   (partition 2 (interleave verticies
                                            (concat (rest verticies)
                                                    [(first verticies)]))))]
    (region edges)))

(deftemplate annulus
  {:inner-radius 1 :outer-radius 2 :centre [0 0]}
  (lang/difference (assoc circle :centre centre :radius outer-radius)
                   (assoc circle :centre centre :radius inner-radius)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Compositing Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REVIEW: I'm providing 2 fns here because I want styling to play syntactically
;; with both the threading style of transforms and standard clojure code. I
;; think `with-style` is more readable in any context except affine threading.

(defn with-style [style shape]
  (lang/style style shape))

(defn style [shape style]
  (lang/style style shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Text Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def raw-text
  (lang/map->RawText
   {:text  ""}))

(deftemplate text
  "Single line of text. No wrapping or truncation."
  {:text  ""}
  (-> raw-text
      (assoc :text text)
      (reflect [1 0])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; API import from falloleen.lang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Templates

(defn template?
  "Returns true iff shape is a template."
  [shape]
  (lang/template? shape))

(defn expand-template
  "Returns the expansion for a given shape template."
  [template]
  (lang/expand-template template))

(defn template-expand-all
  "Returns the fully expanded shape by running `expand-template` until the
  result is no longer a template."
  [shape]
  (if (lang/template? shape)
    (recur (lang/expand-template shape))
    shape))

;;;;; Frames

(defn framed?
  "Returns true iff shape has a defined frame."
  [shape]
  ;; REVIEW: This is awkward because some shapes implement Framed, but aren't
  ;; always guaranteed to have a frame. Take the case of a spline which
  ;; generally has a frame, but it's possible that one of the segments is
  ;; something novel and a frame can't be computed. So having a frame and being
  ;; able to compute the frame aren't the same thing.
  (and (lang/framed? shape) (lang/frame shape)))

(defn frame
  "Returns a frame for shape. A frame is three vectors which are the corner,
  width, and height vectors of a rectangle which fully contains the shape,"
  [shape]
  (if (framed? shape)
    (cond
      (satisfies? lang/Framed shape) (lang/frame shape)
      (template? shape)              (frame (lang/expand-template shape))
      :else                          nil)))

(defn closed?
  "Returns true iff shape has no boundary."
  [shape]
  (lang/closed? shape))

(defn boundary
  "Returns the boundary of a shape, or nil if it hasn't got one."
  [shape]
  (when (satisfies? lang/Compact shape)
    (lang/boundary shape)))

(defn reduce-transformed [shape]
  (lang/apply-transform (.-base shape) (lang/matrix shape)))

(defn frame-point [frame point]
  (lang/frame-point frame point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; And Do Something
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn draw!
  "Draws shape to host. The host determines what drawing means. Return value
  isn't generally meaningful."
  [shape host]
  (lang/render host
               (lang/stack-transform shape
                   (lang/affine 1 0 0 -1 0 (lang/height host))

                   #_(reflect [1 0])
                   #_(translate [0 (lang/height host)]))))
