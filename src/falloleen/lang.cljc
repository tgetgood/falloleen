(ns falloleen.lang
    (:require [falloleen.math :as math :refer [mm v+ v- v*]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Transformable
  "Shapes that know how to apply affine transformations to themselves."
  (apply-transform [this xform]))

(defprotocol LinearTransformation
  (matrix [this] "Returns the matrix form of this 2d linear transform."))

(defprotocol AffineTransformation
  (atx [this]
    "Return the 2d Affine Transformation matrix in the form [a b c d x y"))

(defprotocol IFixedTranslation
  (offset [this]))

(defprotocol IRelativeTranslation
  "Translations relative to a shape: :centre, :bottom-left, etc.."
  (realise [this frame]))

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
    "Returns a triple of vectors representing an offset (origin) and a basis."))

(defprotocol CompilationCache
  "Shapes that can cache their compiled rendering instructions."
  (retrieve [this])
  (store [this v]))

(defprotocol Host
  (base [this] "Returns underlying object.")
  (width [this] "Returns current width of the window.")
  (height [this] "Returns current height of the window".)
  (render [this shape] "Render shape to this host."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; type checkers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^boolean framed? [shape]
  (cond
    (satisfies? Framed shape)     true
    (satisfies? IContainer shape) (framed? (contents shape))
    (template? shape)             (framed? (expand-template shape))
    :else                         false))

(defn ^boolean template? [shape]
  (satisfies? ITemplate shape))

(defn closed?
  "Returns true iff shape has no boundary."
  [shape]
  (or (not (satisfies? Compact shape)) (empty? (boundary shape))))

(defn interior [shape]
  ;; TODO: Just subtract the boundary from the whole thing.
  ;; REVIEW: I haven't found a use for this yet, so maybe should just delete it.
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Relative logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti point-of-box
  "Locate the named point inside the given bounding box. The point can be a
  keyword such as :centre, :top, :bottom-left, etc.. or a vector of the form
  [:relative [s t]] where 0 <= s, t <= 1 which gives a point relative to the
  edges of the box. If the box is [[0 0] [1 0] [0 1]] then [:relative [s t]]
  whould be the point [s t]."
  (fn [k b] k))

(defmethod point-of-box :centre
  [_ [a b c]]
  (v+ a (v* 0.5 (v+ b c))))

(defmethod point-of-box :bottom-left
  [_ [a b c]]
  a)

(defmethod point-of-box :top-right
  [_ [a b c]]
  (v+ a b c))

(defmethod point-of-box :default
  [k [a b c]]
  (when (and (vector? k) (= :relative (first k)))
    (let [[s t] (second k)]
      (v+ a (v* s b) (v* s c)))))

(defn relative?
  "Returns true iff k is a valid relative position."
  [k]
  (or (contains? (methods point-of-box) k)
      (and (vector? k) (= :relative (first k))
           (let [v (second k)]
             (and (vector? v)
                  (= 2 (count v))
                  (every? number? v)
                  (every? #(<= 0 % 1) v))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Affine Transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype TransformedShape [base stack
                           ^:volatile-mutable frame-cache
                           ^:volatile-mutable compile-cache]
  CompilationCache
  (retrieve [_] compile-cache)
  (store [_ v] (set! compile-cache v))

  Framed
  (frame [this]
    (if frame-cache
      frame-cache
      (when (framed? base)
        (let [bf    (frame base)
              xform (atx this)
              f     (mapv #(math/apply-atx xform %) bf)]
          (set! frame-cache f)
          f))))

  IContainer
  (contents [_]
    base)

  AffineTransformation
  (atx [_]
    (transduce (map atx) math/comp-atx (reverse stack))))

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
  (atx [_]
    [1 0 0 1 x y])

  IFixedTranslation
  (offset [_]
    [x y]))

(defrecord RelativeTranslation [k reverse?]
  IRelativeTranslation
  (realise [_ box]
    (let [[x y] (point-of-box k box)]
      (if reverse?
        [(- x) (- y)]
        [x y]))))

(defn translation [v]
  (if (relative? v)
    (RelativeTranslation. v false)
    (let [[x y] v]
      (FixedTranslation. x y))))

(defn- reverse-translation [v]
  (if (relative? v)
    (RelativeTranslation. v true)
    (let [[x y] v]
      (FixedTranslation. (- x) (- y)))))

(defn transform-with-centre
  "Returns a transformed shape which applies linear transform xform around
  centre. This is equivalent to translating the origin to centre, pllying the
  transformation, and then translating back. If centre is the origin, just
  applies xform."
  [shape centre xform]
  (if (= centre [0 0])
    (stack-transform shape xform)
    (stack-transform shape
                     (translation centre)
                     xform
                     (reverse-translation centre))))

;;;;; Reflection

(defrecord Reflection [x y]
  AffineTransformation
  (atx [this]
    (let [[a b c d] (matrix this)]
      [a b c d 0 0]))

  LinearTransformation
  (matrix [_]
    (let [m  (/ y x)
          m2   (* m m)
          m2+1 (inc m2)
          diag (/ (- 1 m2) m2+1)
          off  (/ (* 2 m) m2+1)]
      [diag off off (- diag)])))

(defn reflection [[x y]]
  (Reflection. x y))

;;;;; Scaling

(defrecord Scaling [x y]
  AffineTransformation
  (atx [this]
    (let [[a b c d] (matrix this)]
      [a b c d 0 0]))

  LinearTransformation
  (matrix [_]
    [x 0 0 y]))

(defn scaling [e]
  (cond
    (vector? e) (Scaling. (nth e 0) (nth e 1))
    (number? e) (Scaling. e e)
    :else       nil))

;;;;; Rotation

(defrecord Rotation [angle]
  AffineTransformation
  (atx [this]
    (let [[a b c d] (matrix this)]
      [a b c d 0 0]))

  LinearTransformation
  (matrix [_]
    (let [r (math/deg->rad angle)
          c (math/cos r)
          s (math/sin r)]
      [c (- s) s c])))

(defn rotation [angle]
  (Rotation. angle))

;;;;; Arbitrary Affine Transformation

(defrecord AffineTransform [m]
  AffineTransformation
  (atx [_] m))

(defn build-atx [{[a b c d] :matrix [x y] :translation}]
  (AffineTransform. [a b c d x y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Line [from to]
  Framed
  (frame [_]
    (let [[dx dy] (v- to from)]
      [from [dx 0] [0 dy]]))

  Compact
  (dimension [_] 1)
  (boundary [_] [from to]))

(defrecord Bezier [from to c1 c2]
  ;; TODO: Framed. Not that hard.

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
  ;; TODO: Framed
  Compact
  (dimension [_] 1)
  (boundary [_]
    (when ((< (math/abs (- from to)) (* 2 math/pi)))
      (->> [from to]
           (map (juxt math/cos math/sin))
           (map #(v* radius %))
           (mapv #(v+ % centre))))))

(defrecord Circle [centre radius]
  Compact
  (dimension [_] 2)
  (boundary [_] (Arc. centre radius 0 (* 2 math/pi) false))

  Framed
  (frame [_]
    (let [[x y] centre]
      [[(- x radius) (- y radius)]
       [(* 2 radius) 0]
       [0 (* 2 radius)]])))

(defrecord Spline [segments]
  Framed
  (frame [_]
    (when (every? framed? segments)))
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
  Compact
  (dimension [_] 2)
  (boundary [_] (Spline. segments)))

(defn spline [segs]
  ;; FIXME: Here's where we enforce connected segments.
  (Spline. segs))

(defn closed-spline [segs]
  ;; FIXME: Enforce closed connected path
  (ClosedSpline. segs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Composites
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Style [style base])

(defn style [style base]
  (Style. style base))

;; Do we really need composites? Can't we just use vectors?
(defrecord Composite [contents])

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
