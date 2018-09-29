(ns falloleen.lang
  (:refer-clojure :exclude [compile])
  (:require [falloleen.math :as math :refer [v+ v- v*]]
            [falloleen.util :as util :include-macros true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Transformations

(defprotocol IAffineTransformation
  (matrix [this frame]
    "Return the 2d Affine Transformation matrix in the form [a b c d x y]. Frame
  is provided to allow for relative coordinate systems.")
  (move-point [this frame vector]))

;;;;; Shapes

(defprotocol IShape
  "Basic properties of anything geometric."
  (dimension [this] "Euclidean dimension.")
  (boundary [this] "Returns the boundary of the shape which can be empty."))

(defprotocol Affine
  "Shapes that know how to apply affine transformations to themselves."
  (transform [this xform frame]))

(defprotocol IContainer
  "Uniform access to contents of shape containers."
  (contents [this]))

(defprotocol ITemplate
  "Macros for shapes."
  (expand-template [this]))

(defprotocol Bounded
  "Compact sets have more properties than this, but I haven't found a need for
  them yet."
  (extent [this]
    "Returns a rectangle which fully encloses this shape. The rectangle does not
  have to be minimal, but the closer you can get, the better the results will
  generally be."))

(defprotocol Compilable
  (compile [this compiler]))

;;;;; External Interface

(defprotocol Host
  "A host is anything capable of rendering an image to a screen (or to paper I
  suppose)."
  (dimensions [this] "Returns current dimensions of the window.")
  (render [this shape] "Render shape to this host."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; type checkers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn template? [shape]
  (satisfies? ITemplate shape))

(defn wrap-expander [pred f shape]
  (cond
    (pred shape)                  (f shape)
    (satisfies? IContainer shape) (recur pred f (contents shape))
    (template? shape)             (recur pred f (expand-template shape))
    :else                         nil))

(defn compact? [shape]
  (wrap-expander (fn [x] (satisfies? Bounded x)) (constantly true) shape))

(defn closed?
  "Returns true iff shape is compact and has no boundary."
  [shape]
  (and (compact? shape) (empty? (boundary shape))))

(defn extent* [shape]
  (wrap-expander #(satisfies? Bounded %) extent shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Frames and Relative locations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare spline)
(declare line)

(defn verticies
  "Returns a vector containing the verticies if the given coordinate frame."
  [{:keys [origin a b]}]
  [origin (v+ origin a) (v+ origin a b) (v+ origin b)])

(defrecord CoordinateFrame [origin a b]
  ITemplate
  (expand-template [this]
    (let [vs (verticies this)]
      (spline (map line vs (rest (cycle vs))))))

  Bounded
  (extent [this] this)

  Affine
  (transform [this xform f]
    (let [oa' (move-point xform f (v+ a origin))
          ob' (move-point xform f (v+ b origin))
          o'  (move-point xform f origin)]
      (CoordinateFrame. o'
                        (v- oa' o')
                        (v- ob' o')))))


(defn coordinate-frame [origin a b]
  (CoordinateFrame. origin a b))

;; TODO: just use spec.

(defn v2d? [v]
  (and (vector? v)
       (= 2 (count v))
       (every? number? v)))

(defn valid-relative? [v]
  (and (sequential? v)
       (= 2 (count v))
       (= :relative (first v))
       (v2d? (second v))
       (every? #(<= 0 % 1) (second v))))

(def position-map
  {:centre       [0.5 0.5]
   :bottom-left  [0 0]
   :bottom-right [1 0]
   :top-right    [1 1]
   :top-left     [0 1]
   :top          [0.5 1]
   :left         [0 0.5]
   :right        [1 0.5]
   :bottom       [0.5 0]})

(defn relative-coords [[s t] {:keys [origin a b]}]
  (v+ origin (v* s a) (v* t b)))

(defn point-in-frame
  "Given a coordinate frame and a relative point, return the respective absolute
  point."
  [f p]
  (if (keyword? p)
    (relative-coords (get position-map p) f)
    (relative-coords p f)))

(defn relative-vector
  "Returns true iff k is a valid relative position."
  [k]
  (if (contains? position-map k)
    (get position-map k)
    (when (valid-relative? k)
      (second k))))

(defn bound-points
  "Given a seq of points, return the smallest rectangle (aligned with the axes)
  that contains all of them."
  [ps]
  ;; FIXME: This always returns a rectangle. We could alomst certainly get a
  ;; better fit if we circumscribed the convex hull with a parallelogram. I
  ;; don't have an algorithm for that at the moment though.
  (let [[[x1 y1] [x2 y2]] (math/bound-points ps)]
    (CoordinateFrame. [x1 y1] [(- x2 x1) 0] [0 (- y2 y1)])))

(defn bound-all
  "Given a sequence of rectangles, returns the smalled single rectangle that
  contains all of them."
  [rects]
  (bound-points (mapcat verticies rects)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Affine Transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype AffineWrapper [shape xform
                        ^:volatile-mutable cache]

  IShape
  (dimension [_]
    (dimension shape))
  (boundary [_]
    (AffineWrapper. (boundary shape) xform nil))

  Bounded
  (extent [_]
    (when (compact? shape)
      (let [coords (extent* shape)]
        (transform coords xform coords))))

  Compilable
  (compile [this compiler]
    (if cache
      cache
      (let [code (compiler this)]
        (set! cache code)
        code))))

(defn aw? [x]
  (instance? AffineWrapper x))

(defn aw-matrix [^AffineWrapper aw]
  (matrix (.-xform aw) (extent* (.-shape aw))))

(defn wrap-affine [shape xform]
  (AffineWrapper. shape xform nil))

;;;;; Arbitrary Affine Transformation

(defrecord RawAffineTransformation [a b c d x y]
  IAffineTransformation
  (matrix [_ _] [a b c d x y])
  (move-point [_ _ v]
    (math/apply-atx [a b c d x y] v)))

(defn affine-transformation
  ([[a b c d e f]]
   (RawAffineTransformation. a b c d e f))
  ([a b c d e f]
   (RawAffineTransformation. a b c d e f)))

(defn build-atx [{[a b c d] :matrix [x y] :translation}]
  (RawAffineTransformation. a b c d x y))

;;;;; Translation

(defrecord FixedTranslation [x y]
  IAffineTransformation
  (matrix [_ _]
    [1 0 0 1 x y])
  (move-point [_ _ [p q]]
    [(+ p x) (+ q y)]))

(defn reverse-translation [{:keys [x y]}]
  (FixedTranslation. (- x) (- y)))

(defrecord RelativeTranslation [k]
  IAffineTransformation
  (matrix [_ box]
    (let [[x y] (relative-coords k box)]
      [1 0 0 1 x y]))
  (move-point [_ f v]
    (mapv + v (relative-coords k f))))

(defn translation [v]
  (if-let [v' (relative-vector v)]
    (RelativeTranslation. v')
    (FixedTranslation. (nth v 0) (nth v 1))))

(defrecord RecentredLinearTransform [translation linear]
  IAffineTransformation
  (matrix [_ _]
    (let [tx (matrix translation nil)
          rtx (matrix (reverse-translation translation) nil)]
      (math/comp-atx tx (matrix linear nil) rtx)))
  (move-point [this f v]
    (math/apply-atx (matrix this f) v)))

(defrecord RelativeRecentredLinearTransform [reltrans linear]
  IAffineTransformation
  (matrix [_ frame]
    (let [trans (affine-transformation (matrix reltrans frame))]
      (matrix (RecentredLinearTransform. trans linear) nil)))
  (move-point [this f v]
    (math/apply-atx (matrix this f) v)))

(defn transform-with-centre
  "Returns a transformed shape which applies linear transform xform around
  centre. This is equivalent to translating the origin to centre, pllying the
  transformation, and then translating back. If centre is the origin, just
  applies xform."
  [shape centre xform]
  (if (= centre [0 0])
    (wrap-affine shape xform)
    (let [t (translation centre)]
      (if (relative-vector centre)
        (wrap-affine shape (RelativeRecentredLinearTransform. t xform))
        (wrap-affine shape (RecentredLinearTransform. t xform))))))

;;;;; Reflection

(defrecord Reflection [x y]
  IAffineTransformation
  (matrix [_ _]
    (if (zero? x)
      [-1 0 0 1 0 0]
      (let [m  (/ y x)
            m2   (* m m)
            m2+1 (inc m2)
            diag (/ (- 1 m2) m2+1)
            off  (/ (* 2 m) m2+1)]
        [diag off off (- diag) 0 0])))
  (move-point [this f p]
    (math/apply-atx (matrix this f) p)))

(defn reflection [[x y]]
  (Reflection. x y))

;;;;; Scaling

(defrecord Scaling [x y]
  IAffineTransformation
  (move-point [_ _ [p q]]
    [(* x p) (* y q)])
  (matrix [_ _]
    [x 0 0 y 0 0]))

(defn scaling [e]
  (cond
    (vector? e) (Scaling. (nth e 0) (nth e 1))
    (number? e) (Scaling. e e)
    :else       nil))

;;;;; Rotation

(defrecord Rotation [angle]
  IAffineTransformation
  (matrix [_ _]
    (let [r (math/deg->rad angle)
          c (math/cos r)
          s (math/sin r)]
      [c (- s) s c 0 0]))
    (move-point [this f p]
    (math/apply-atx (matrix this f) p)))

(defn rotation [angle]
  (Rotation. angle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Line [from to]
  Affine
;; FIXME: transform should be relative to the frame of reference of the
;; transformation or the transformed?
  (transform [this xform f]
    (Line. (move-point xform f from) (move-point xform f to)))

  Bounded
  (extent [_]
    (bound-points [from to]))

  IShape
  (dimension [_] 1)
  (boundary [_] [from to]))

(defn line [f t]
  (Line. f t))

(defrecord Bezier [from to c1 c2]
  Affine
  (transform [this xform box]
    (let [[f' t' c1' c2'] (map #(move-point xform box %) [from to c1 c2])]
      (Bezier. f' t' c1' c2')))

  Bounded
  (extent [_]
    (bound-points [from to c1 c2]))

  IShape
  (dimension [_] 1)
  (boundary [_] [from to]))

(defrecord Arc [centre radius from to clockwise?]
  Bounded
  (extent [_]
    ;; TODO: Refine. We can do a lot better than this in general.
    (bound-points ((juxt v- v+) centre [radius radius])))

  IShape
  (dimension [_] 1)
  (boundary [_]
    (when (< (math/abs (- from to)) (* 2 math/pi))
      (->> [from to]
           (map (juxt math/cos math/sin))
           (map #(v* radius %))
           (mapv #(v+ % centre))))))

(defrecord Circle [centre radius]
  Affine
  (transform [this xform f]
    (let [c' (move-point xform f centre)
          r* (move-point xform f (mapv + centre [radius 0]))
          r' (math/dist c' r*)]
      (Circle. c' r')))

  IShape
  (dimension [_] 2)
  (boundary [_] (Arc. centre radius 0 (* 2 math/pi) false))

  Bounded
  (extent [_]
    (bound-points ((juxt v- v+) centre [radius radius]))))

(defrecord Spline [segments]
  Affine
  (transform [_ xform f]
    ;; REVIEW: Will this work for relative coords? What are we operating
    ;; relative to? I think this is broken
    (Spline. (map #(transform % xform f) segments)))

  Bounded
  (extent [_]
    (when (every? compact? segments)
      (bound-all (map extent segments))))

  IShape
  (dimension [_] 1)
  (boundary [_]
    (let [a (first (boundary (first segments)))
          b (last (boundary (last segments)))]
      (when-not (= a b)
        [a b]))))

(defrecord ClosedSpline [segments]
  Affine
  (transform [_ xform f]
    (ClosedSpline.
     (map #(transform % xform f) segments)))

  Bounded
  (extent [_]
    (when (every? compact? segments)
      (bound-all (map extent segments))))

  IShape
  (dimension [_] 2)
  (boundary [_] (Spline. segments)))

(defn spline [segs]
  ;; FIXME: Here's where we enforce connected segments.
  (Spline. segs))

(defn closed-spline [segs]
  ;; FIXME: Enforce closed connected path
  (ClosedSpline. segs))

(defrecord Rectangle [origin width height]
  Affine
  (transform [this xform f]
    (let [[x y] origin
          o' (move-point xform f origin)
          w' (move-point xform f [(+ x  width) y])
          h' (move-point xform f [x (+ y height)])
          verticies [o' (map + o' w') (map + o' w' h') (map + o' h')]]
      (ClosedSpline.
       (map #(Line. %1 %2) verticies (rest (cycle verticies))))))

  Bounded
  (extent [this]
    (coordinate-frame origin [width 0] [0 height]))

  ITemplate
  (expand-template [_]
    (let [[x1 y1] origin
          x2      (+ x1 width)
          y2      (+ y1 height)
          verticies [[x1 y1] [x2 y1] [x2 y2] [x1 y2]]]
    (closed-spline
     (map #(Line. %1 %2) verticies (rest (cycle verticies)))))))

(def rectangle
  (map->Rectangle
   {:origin [0 0]
    :width 1
    :height 1}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Style [style shape]
  IShape
  (dimension [_] (dimension shape))
  (boundary [_]
    (Style. style (boundary shape)))

  IContainer
  (contents [_]
    shape)

  Affine
  (transform [_ xform f]
    (Style. style (transform shape xform f)))

  Bounded
  (extent [_]
    (when (compact? shape)
      (extent shape))))

(defn style [style shape]
  (Style. style shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note that since we're inverting coordinates systematically to get back to the
;; Cartesian plane, raw text renders upside down. This is easily fixed by the
;; `text` template in core.
(defrecord RawText [text])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Shape Algebra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REVIEW: Do I need to implement clipping, or should the idiom be to intersect
;; with a transparent shape? That intersection will be implemented as some kind
;; of clipping, but that's another level of abstraction.
(defrecord Union [shapes])

(defrecord Intersection [shapes])

(defrecord Difference [a b])

(defn difference [a b]
  (Difference. a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Default protocol impls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-protocol Affine
  #?(:clj Object :cljs default)
  (transform [this xform f]
    (if (template? this)
      (transform (expand-template this) xform f)
      (println "I don't know how to transform a " (type this)))))

(util/implement-sequentials
 Affine
 (transform [this xform f]
   (map #(transform % xform f) this)))

(util/implement-sequentials
  Bounded
  (extent [this]
    (when (every? compact? this)
      (bound-all (map extent* this)))))
