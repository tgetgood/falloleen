(ns falloleen.core
  (:require [falloleen.math :as math :refer [mm v+ v*]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Bounded
  "Shapes that are bounded in space."
  (bounding-box [this]
    "Returns a triple of vectors representing an offset (origin) and a basis."))

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
  (endpoints [this]))

(defprotocol Boundary
  "Curves which form the boundary of 2D shapes (are connected and closed)."
  (interior [this]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare stack-transform)

;;;;; Relative logic

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

(defrecord Reflection [x y]
  LinearTransformation
  (matrix [_]
    (let [m  (/ y x)
          m2   (* m m)
          m2+1 (inc m2)
          diag (/ (- 1 m2) m2+1)
          off  (/ (* 2 m) m2+1)]
      [diag off off (- diag)])))

(defrecord Scaling [x y]
  LinearTransformation
  (matrix [_]
    [x 0 0 y]))

(defrecord Rotation [angle]
  LinearTransformation
  (matrix [_]
    (let [r (math/deg->rad angle)
          c (math/cos r)
          s (math/sin r)]
      [c (- s) s c])))

(defrecord AffineTransform [a b c d x y])

(defn atxv [xform v]
  (cond
    (satisfies? LinearTransformation xform)
    (mm (matrix xform) v)

    (satisfies? ITranslation xform)
    (mapv + (offset xform) v)

    (instance? AffineTransform xform)
    (let [{:keys [a b c d x y]} xform]
      (mapv + [x y] (mm [a b c d] v)))

    :else
    ;; TODO: Handle errors
    nil))

(defn relative? [k]
  (or (keyword? k)
      (and (vector? k) (= :relative (first k)))))

(defn translation [v]
  (if (relative? v)
    (RelativeTranslation. v false)
    (let [[x y] v]
      (FixedTranslation. x y))))

(defn bt [v]
  (if (relative? v)
    (RelativeTranslation. v true)
    (let [[x y] v]
      (FixedTranslation. (- x) (- y)))))

(defn rotation [angle]
  (Rotation. angle))

(defn scaling [e]
  (cond
    (vector? e) (Scaling. (nth e 0) (nth e 1))
    (number? e) (Scaling. e e)
    :else       nil))

(defn reflection [[x y]]
  (Reflection. x y))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; ATX API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translate [shape v]
  (stack-transform shape (translation v)))

(defn rotate
  ([shape angle]
   (stack-transform shape (rotation angle)))
  ([shape centre angle]
   (stack-transform shape (translation centre) (rotation angle) (bt centre))))

(defn scale
  ([shape extent]
   (stack-transform shape (scaling extent)))
  ([shape centre extent]
   (stack-transform shape (translation centre) (scaling extent) (bt centre))))

(defn reflect
  ([shape axis]
   (stack-transform shape (reflection axis)))
  ([shape centre axis]
   (stack-transform shape (translation centre) (reflection axis) (bt centre))))

(defn transform [shape {[a b c d] :matrix [x y] :translation}]
  (stack-transform shape (AffineTransform. a b c d x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; browser testing temp code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#?(:cljs (enable-console-print!))

(println "This text is printed from src/falloleen/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))

(defn init [])

(defn on-js-reload []
  (init))

(init)
