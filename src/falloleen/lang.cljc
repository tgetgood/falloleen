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

(defprotocol ICurve
  "1 dimensional visual object."
  (endpoints [this])
  (boundary? [this]
    "Returns true iff this curve is the boundary of a shape. Note that this
    implies that this curve must be connected and closed.")
  (interior [this]
    "Returns the shape corresponding to the interior of this curve, if it
    exists."))

(defprotocol IShape
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
