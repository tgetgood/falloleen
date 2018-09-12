(ns falloleen.lang
    (:require [falloleen.math :as math :refer [v+ v- v*]]
              [falloleen.util :as util :include-macros true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Transformable
  "Shapes that know how to apply affine transformations to themselves."
  (apply-transform [this xform]))

(defprotocol AffineTransformation
  (matrix [this]
    "Return the 2d Affine Transformation matrix in the form [a b c d x y"))

(defprotocol IRelative
  "Translations relative to a shape: :centre, :bottom-left, etc.."
  (fix-coords [this frame]))

(defprotocol IContainer
  "Uniform access to contents of shape containers."
  (contents [this]))

(defprotocol ITemplate
  "Macros for shapes."
  (expand-template [this]))

(defprotocol Compact
  "Shapes which are finite in extent and contain their boundary (if they have
  one). Compact manifolds are the basic building blocks of our system.
  I haven't found a useful reason to be able to refer to the interior of a
  compact manifold, so I haven't added it."
  (dimension [this])
  (boundary [this]))

(defprotocol Framed
  "Shapes that are bounded in space."
  (frame [this]
    "Returns a rectangle which fully encloses this shape. The rectangle does not
    have to be minimal, but the closer you can get, the better the results will
    generally be."))

(defprotocol CompilationCache
  "Shapes that can cache their compiled rendering instructions."
  (retrieve [this])
  (store [this v]))

(defprotocol Host
  (base [this] "Returns underlying object.")
  (width [this] "Returns current width of the window.")
  (height [this] "Returns current height of the window".)
  (render [this shape] "Render shape to this host."))

(util/implement-sequentials
 Transformable
 (apply-transform [this xform]
                  (map #(apply-transform % xform) this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; type checkers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn template? [shape]
  (satisfies? ITemplate shape))

(defn framed? [shape]
  (cond
    (satisfies? Framed shape)     true
    (satisfies? IContainer shape) (framed? (contents shape))
    (template? shape)             (framed? (expand-template shape))
    :else                         false))

(defn closed?
  "Returns true iff shape has no boundary."
  [shape]
  (or (not (satisfies? Compact shape)) (empty? (boundary shape))))

(defn interior [shape]
  ;; TODO: Just subtract the boundary from the whole thing.
  ;; REVIEW: I haven't found a use for this yet, so maybe should just delete it.
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Frames and Relative locations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare rectangle)

(defn v2d? [v]
  (and (vector? v)
       (= 2 (count v))
       (every? number? v)))

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

(defn relative-coords [[s t] {[x y] :origin w :width h :height}]
  [(+ x (* s w)) (+ y (* t h))])

(defn frame-point [f p]
  (if (keyword? p)
    (relative-coords (get position-map p) f)
    (relative-coords p f)))

(defn valid-relative? [v]
  ;; TODO: just use spec.
  (and (sequential? v)
       (= 2 (count v))
       (= :relative (first v))
       (v2d? (second v))
       (every? #(<= 0 % 1) (second v))))

(defn relative-vector
  "Returns true iff k is a valid relative position."
  [k]
  (if (contains? position-map k)
    (get position-map k)
    (when (valid-relative? k)
      (second k))))

(defn frame-points
  "Given a seq of points, return the smallest rectangle (aligned with the axes)
  that contains all of them."
  [ps]
  (let [[[x1 y1] [x2 y2]] (math/bound-points ps)]
    (assoc rectangle :origin [x1 y1] :width (- x2 x1) :height (- y2 y1))))

(defn verticies [{[x y] :origin w :width h :height}]
  (let [x2 (+ x w)
        y2 (+ y h)]
    [[x y] [x2 y] [x2 y2] [x y2]]))

(defn frame-rects
  "Given a sequence of rectangles, returns the smalled single rectangle that
  contains all of them."
  [rects]
  (frame-points (mapcat verticies rects)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Affine Transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn frame-transform-step [{:keys [xform shape]} o]
  (let [sx     (if (satisfies? AffineTransformation o)
                 (matrix o)
                 (matrix (fix-coords o (frame shape))))]
    {:shape (apply-transform shape sx)
     :xform (math/comp-atx xform sx)}))

(deftype TransformedShape [base stack
                           ^:volatile-mutable xform
                           ^:volatile-mutable compile-cache]
  Transformable
  (apply-transform [_ xform]
    (TransformedShape. base (conj stack xform) nil nil))

  CompilationCache
  (retrieve [_] compile-cache)
  (store [_ v] (set! compile-cache v))

  Framed
  (frame [this]
    (when (framed? base)
      (frame (apply-transform base (matrix this)))))

  IContainer
  (contents [_]
    base)

  AffineTransformation
  (matrix [_]
    (if xform
      xform
      (let [state (reduce frame-transform-step
                          {:shape base :xform [1 0 0 1 0 0]}
                          stack)]
        (set! xform (:xform state))
        xform))))

(defn transformed [base stack]
  (TransformedShape. base stack nil nil))

(defn transformed? [shape]
  (instance? TransformedShape shape))

(defn stack-transform
  ([shape xform]
   (if (transformed? shape)
     (transformed (contents shape) (conj (.-stack shape) xform))
     (transformed shape [xform])))
  ([shape xform & xforms]
   (let [base (stack-transform shape xform)]
     (transformed (.-base base) (into (.-stack base) xforms)))))

;;;;; Translation

(defrecord FixedTranslation [x y]
  AffineTransformation
  (matrix [_]
    [1 0 0 1 x y]))

(defn reverse-translation [{:keys [x y]}]
  (FixedTranslation. (- x) (- y)))

(defrecord RelativeTranslation [k]
  IRelative
  (fix-coords [_ box]
    (let [[x y] (relative-coords k box)]
      (FixedTranslation. x y))))

(defn translation [v]
  (if-let [v' (relative-vector v)]
    (RelativeTranslation. v')
    (FixedTranslation. (nth v 0) (nth v 1))))

(defrecord RecentredLinearTransform [translation linear]
  AffineTransformation
  (matrix [_]
    (let [tx (matrix translation)
          rtx (matrix (reverse-translation translation))]
      (math/comp-atx tx (matrix linear) rtx))))

(defrecord RelativeRecentredLinearTransform [reltrans linear]
  IRelative
  (fix-coords [_ frame]
    (let [trans (fix-coords reltrans frame)]
      (RecentredLinearTransform. trans linear))))

(defn transform-with-centre
  "Returns a transformed shape which applies linear transform xform around
  centre. This is equivalent to translating the origin to centre, pllying the
  transformation, and then translating back. If centre is the origin, just
  applies xform."
  [shape centre xform]
  (if (= centre [0 0])
    (stack-transform shape xform)
    (let [t (translation centre)]
      (if (relative-vector centre)
        (stack-transform shape (RelativeRecentredLinearTransform. t xform))
        (stack-transform shape (RecentredLinearTransform. t xform))))))

;;;;; Reflection

(defrecord Reflection [x y]
  AffineTransformation
  (matrix [_]
    (if (zero? x)
      [-1 0 0 1 0 0]
      (let [m  (/ y x)
            m2   (* m m)
            m2+1 (inc m2)
            diag (/ (- 1 m2) m2+1)
            off  (/ (* 2 m) m2+1)]
        [diag off off (- diag) 0 0]))))

(defn reflection [[x y]]
  (Reflection. x y))

;;;;; Scaling

(defrecord Scaling [x y]
  AffineTransformation
  (matrix [_]
    [x 0 0 y 0 0]))

(defn scaling [e]
  (cond
    (vector? e) (Scaling. (nth e 0) (nth e 1))
    (number? e) (Scaling. e e)
    :else       nil))

;;;;; Rotation

(defrecord Rotation [angle]
  AffineTransformation
  (matrix [_]
    (let [r (math/deg->rad angle)
          c (math/cos r)
          s (math/sin r)]
      [c s (- s) c 0 0])))

(defn rotation [angle]
  (Rotation. angle))

;;;;; Arbitrary Affine Transformation

(defrecord AffineTransform [a b c d x y]
  AffineTransformation
  (matrix [_] [a b c d x y]))

(defn affine [a b c d e f]
  (AffineTransform. a b c d e f))

(defn build-atx [{[a b c d] :matrix [x y] :translation}]
  (AffineTransform. a b c d x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Line [from to]
  Transformable
  (apply-transform [_ xform]
    (Line. (math/apply-atx xform from) (math/apply-atx xform to)))

  Framed
  (frame [_]
    (let [[dx dy] (v- to from)]
      (assoc rectangle :origin from :width dx :height dy)))

  Compact
  (dimension [_] 1)
  (boundary [_] [from to]))

(defrecord Bezier [from to c1 c2]
  Transformable
  (apply-transform [_ xform]
    (let [[f' t' c1' c2'] (map #(math/apply-atx xform %) [from to c1 c2])]
      (Bezier. f' t' c1' c2')))

  Framed
  (frame [_]
    ;; REVIEW: This is correct, but there's probably a tighter bound to be had.
    (frame-points [from to c1 c2]))

  Compact
  (dimension [_] 1)
  (boundary [_] [from to]))

;; REVIEW: This is awkward. A circle in geometry is a unit. When you think about
;; circles, you don't want to think about whether they go clockwise or anti-,
;; nor where the 'joining point' is.
;;
;; That's because those things don't need to be specified in a circle and so at
;; the level of abstraction at which we generally operate, those things are not
;; defined.
;;
;; This problem is that if you take the difference of two nested circles you
;; should get an annulus, but I don't know of any drawing language that can do
;; that without specifying that the circles travel in opposite directions.
;;
;; A good solution would be to add a solver that figures out a set of paths
;; which have the correct winding number. The problem is that 1) that's hard,
;; and 2) you're now computing something which the gpu has to go and
;; invert. That's wasteful. There has to be a better way.
;;
;; I'm not going to compromise and let proceedural details bleed into the high
;; level logic this time around. It was a mistake last time and even though I
;; don't see a solution, I'd rather wait one out than wind up in that mess
;; again.

(defrecord Arc [centre radius from to clockwise?]
  Framed
  (frame [_]
    ;; TODO: Refine. We can do a lot better than this in general.
    (let [[x y] centre]
      (assoc rectangle
             :origin [(- x radius) (- y radius)]
             :width  (* 2 radius)
             :height (* 2 radius))))

  Compact
  (dimension [_] 1)
  (boundary [_]
    (when ((< (math/abs (- from to)) (* 2 math/pi)))
      (->> [from to]
           (map (juxt math/cos math/sin))
           (map #(v* radius %))
           (mapv #(v+ % centre))))))

(defrecord Circle [centre radius]
  Transformable
  (apply-transform [_ xform]
    (let [c' (math/apply-atx xform centre)
          r* (math/apply-atx xform (mapv + centre [radius 0]))
          r' (math/dist c' r*)]
      (Circle. c' r')))

  Compact
  (dimension [_] 2)
  (boundary [_] (Arc. centre radius 0 (* 2 math/pi) false))

  Framed
  (frame [_]
    (let [[x y] centre]
      (assoc rectangle
             :origin [(- x radius) (- y radius)]
             :width  (* 2 radius)
             :height (* 2 radius)))))

(defrecord Spline [segments]
  Transformable
  (apply-transform [_ xform]
    (Spline. (map #(apply-transform % xform) segments)))

  Framed
  (frame [_]
    (when (every? framed? segments)
      (frame-rects (map frame segments))))

  Compact
  (dimension [_] 1)
  (boundary [_]
    (let [a (first (boundary (first segments)))
          b (last (boundary (last segments)))]
      (when-not (= a b)
        [a b])))
  ;; REVIEW: Here's where it gets interesting. Some shapes are the boundaries of
  ;; higher dimension shapes. This is one, the arc is another. This isn't a
  ;; property of the kind of shape though, it's a topological peculiarity of the
  ;; particular shape itself.
  ;;
  ;; Topologically if a shape has no boundary, then it is the boundary of a
  ;; shape of one higher dimension. So that plays to the same protocol, because
  ;; each kind of shape has a higher dimensional analog. Maybe that table should
  ;; live somewhere else? I don't know yet.
  )

(defrecord ClosedSpline [segments]
  Transformable
  (apply-transform [_ xform]
    (ClosedSpline.
     (map #(apply-transform % xform) segments)))

  Framed
  (frame [_]
    (when (every? framed? segments)
      (frame-rects (map frame segments))))

  Compact
  (dimension [_] 2)
  (boundary [_] (Spline. segments)))

(defn spline [segs]
  ;; FIXME: Here's where we enforce connected segments.
  (Spline. segs))

(defn closed-spline [segs]
  ;; FIXME: Enforce closed connected path
  (ClosedSpline. segs))

(defrecord Rectangle [origin width height]
  Transformable
  (apply-transform [_ xform]
    (let [[x y] origin
          o' (math/apply-atx xform origin)
          w' (math/apply-atx xform [(+ x  width) y])
          h' (math/apply-atx xform [x (+ y height)])
          verticies [o' (map + o' w') (map + o' w' h') (map + o' h')]]
      (ClosedSpline.
       (map #(Line. %1 %2)
            verticies
            (concat (rest verticies) [(first verticies)])))))

  Framed
  (frame [this]
    this)

  ITemplate
  (expand-template [_]
    (let [[x1 y1] origin
          x2      (+ x1 width)
          y2      (+ y1 height)
          verticies [[x1 y1] [x2 y1] [x2 y2] [x1 y2]]]
    (closed-spline
     (map #(Line. %1 %2)
          verticies
          (concat (rest verticies) [(first verticies)]))))))

(def rectangle
  (map->Rectangle
   {:origin [0 0]
    :width 1
    :height 1}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Style [style base]
  Transformable
  (apply-transform [_ xform]
    (Style. style (transformed base [xform])))

  Framed
  (frame [_]
    (when (framed? base)
      (frame base))))

(defn style [style base]
  (Style. style base))

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
