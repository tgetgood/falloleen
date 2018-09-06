(ns falloleen.lang
    (:require [falloleen.math :as math :refer [mm v+ v*]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Transformable
  "Shapes that know how to apply affine transformations to themselves."
  (apply-transform [this xform]))

(defprotocol LinearTransformation
  (matrix [this] "Returns the matrix form of this 2d linear transform."))

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

(defprotocol Curve
  "1 dimensional visual object."
  (endpoints [this])
  (boundary? [this]
    "Returns true iff this curve is the boundary of a shape. Note that this
    implies that this curve must be connected and closed.")
  (interior [this]
    "Returns the shape corresponding to the interior of this curve, if it
    exists."))

(defprotocol Figure
  "2 dimensional visual object."
  (boundary [this]))

(defprotocol Bounded
  "Shapes that are bounded in space."
  (frame [this]
    "Returns a triple of vectors representing an offset (origin) and a basis."))

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

(deftype Transformed [base stack
                      ^:volatile-mutable frame-cache
                      ^:volatile-mutable compile-cache])

(defn transformed [base stack]
  (Transformed. base stack nil nil))

(defn transformed? [shape]
  (instance? Transformed shape))

(defn stack-transform
  ([shape xform]
   (if (transformed? shape)
     (transformed (.-base shape) (conj (.-stack shape) xform))
     (transformed shape [xform])))
  ([shape xform & xforms]
   (let [base (stack-transform shape xform)]
     (transformed (.-base base) (into (.-stack base) xforms)))))

(defn bounded? [shape]
  (if (transformed? shape)
    (bounded? (.-base shape))
    (satisfies? Bounded shape)))


;;;;; Translation

(defrecord FixedTranslation [x y]
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
  LinearTransformation
  (matrix [_]
    (let [r (math/deg->rad angle)
          c (math/cos r)
          s (math/sin r)]
      [c (- s) s c])))

(defn rotation [angle]
  (Rotation. angle))

;;;;; Arbitrary Affine Transformation

(defrecord AffineTransform [a b c d x y])

(defn atx [{[a b c d] :matrix [x y] :translation}]
  (AffineTransform. a b c d x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Line [from to]
  Curve
  (endpoints [_] [from to])
  (boundary? [_] false)
  (interior [_] nil))

(defrecord Bezier [from to c1 c2]
  Curve
  (endpoints [_] [from to])
  (boundary? [_] false)
  (interior [_] nil))

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
(defrecord Circle [centre radius]
  Figure
  (boundary [this] this)

  Curve
  (endpoints [_] nil)
  (boundary? [_] true)
  (interior [this] this))

(defrecord Arc [centre radius from to clockwise?]
  Curve
  (endpoints [_]
    (when ((< (math/abs (- from to)) (* 2 math/pi)))
      (->> [from to]
           (map (juxt math/cos math/sin))
           (map #(v* radius %))
           (mapv #(v+ % centre)))))
  (boundary? [_]
    (<= (* 2 math/pi) (math/abs (- from to))))
  (interior [this]
    (when (boundary? this)
      (Circle. centre radius))))

(defrecord Spline [segments]
  Curve
  (endpoints [_]
    [(first (endpoints (first segments))) (last (endpoints (last segments)))])
  (boundary? [this]
    (apply = (endpoints this)))
  (interior [this]
    (when (boundary? this)
      (region segments))))

(defrecord ClosedSpline [segments]
  Curve
  (endpoints [_] nil)
  (boundary? [_] true)
  (interior [this] this)

  Figure
  (boundary [this] this))

(defn spline [segs]
  ;; FIXME: Here's where we enforce connected segments.
  (Spline. segs))

(defn closed-spline [segs]
  ;; FIXME: Enforce closed connected path
  (ClosedSpline. segs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Composites
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note that since we're inverting coordinates systematically to get back to the
;; Cartesian plane, raw text renders upside down. This is easily fixed by the
;; `text` template in core.
(defrecord RawText [style text])
