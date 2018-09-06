(ns falloleen.core
  #?@(:clj
       [(:require
         [clojure.string :as string]
         [falloleen.lang :as lang]
         [falloleen.math :as math]
         [net.cgrand.macrovich :as macros :include-macros true])]
       :cljs
       [(:require
         [clojure.string :as string]
         [falloleen.lang :as lang]
         [falloleen.math :as math]
         [net.cgrand.macrovich :as macros :include-macros true])
        (:require-macros [falloleen.core :refer [deftemplate]])]))

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
  (lang/stack-transform shape (lang/atx xform)))

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

(defn ^boolean template? [shape]
  (satisfies? lang/ITemplate shape))

(defn template-expand-all [shape]
  (if (template? shape)
    (recur (lang/expand-template shape))
    shape))

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
    (path segs))

  lang/Bounded
  (frame [{:keys [points]}]
    (let [[[x y] [x' y']] (math/bound-points points)
          dx (- x' x)
          dy (- y' y)]
      [[x y] [dx 0] [0 dy]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REVIEW: I think I'm making a mistake: I've created a duality between
;; boundaries and interiors, which is fine, but in real life you don't
;; care. Everyone --- including mathematicians --- use the term circle to refer
;; to both the boundary and the interior. It's a whole.
;;
;; Should I replace my notion of figures with whole shapes and just let styling
;; determine what the programmer means? After all if you want to draw the
;; boundary of a shape, you're referring to the boundary, if you want to fill
;; it, the interior.
;;
;; The concrete problem is that I want to use rectangle interchangeably to refer
;; to a box (outline) or to a patch (the interior).
;;
;; Do I actually have a problem? After all they're represented by the same
;; boundary. If I just get rid of disc and use circle, and get rid of region in
;; favour of spline, I think my problems will go away.
;;
;; Maybe I do want a second type of ClosedSpline, or some such which would allow
;; me to know whether or not the interior of the spline makes sense?
;;
;; I'm confusing myself. I think I need to come up with some more complex
;; examples to clarify.

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
    (region edges))

  lang/Bounded
  (frame [{:keys [verticies]}]
    (let [[[x y] [x' y']] (math/bound-points verticies)
          dx (- x' x)
          dy (- y' y)]
      [[x y] [dx 0] [0 dy]])))

(deftemplate rectangle
  {:corner [0 0]
   :height 1
   :width  1}
  (let [[x1 y1] corner
        x2      (+ x1 width)
        y2      (+ y1 height)]
    (assoc polygon :verticies [[x1 y1] [x2 y1] [x2 y2] [x1 y2]])))

(deftemplate annulus
  {:inner-radius 1 :outer-radius 2 :centre [0 0]}
  (lang/difference (assoc circle :centre centre :radius outer-radius)
                   (assoc circle :centre centre :radius inner-radius)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Compositing Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn with-style [style shape]
  (lang/style style shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Text Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def raw-text
  (lang/map->RawText
   {:style  {:font "sans serif 10px"}
    :text   ""}))

(deftemplate text
  "Single line of text. No wrapping or truncation."
  {:style {:font "sans serif 10px"}
   :text ""}
  (-> raw-text
      (assoc :text text :style style)
      (reflect [1 0])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; browser testing temp code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare height)
(declare render-fn)

(defn draw!
  "Draws shape to host. The host determines what drawing means. Return value
  isn't generally meaningful."
  [shape host]
  ((render-fn host)
   (-> shape
       (reflect [1 0])
       (translate [0 (- (height host))]))))

#?(:cljs (enable-console-print!))

(println "This text is printed from src/falloleen/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(def test-image
  [(assoc circle :radius 200)
   (translate (assoc text :text "Hi there") [200 200])])

(defn ^:export init []
  (draw! {} test-image))

(defn on-js-reload []
  (init))
