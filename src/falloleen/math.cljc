(ns falloleen.math)

(def π
  #?(:cljs js/Math.PI
     :clj Math/PI))

(def pi
  "Ratio of circumference to diameter of a circle.
  For those who don't like programming with unicode."
  π)
(def e
  #?(:clj Math/E
     :cljs js/Math.E))

(defn deg->rad
  "Converts degrees to radians."
  [d]
  (* π (/ d 180)))

(defn rad->deg
  "Convert radians into degrees."
  [r]
  (/ (* r 180) π))

(defn sin [x]
  (#?(:cljs js/Math.sin :clj Math/sin) x))

(defn cos [x]
  (#?(:cljs js/Math.cos :clj Math/cos) x))

(defn atan [x]
  (#?(:clj Math/atan :cljs js/Math.atan) x))

(defn abs [x]
  (if (< x 0)
    (- x)
    x))

(defn floor [x]
  (#?(:clj Math/floor :cljs js/Math.floor) x))

(defn sqrt [x]
  (#?(:cljs js/Math.sqrt :clj Math/sqrt) x))

(defn nan? [x]
  (#?(:cljs js/isNaN :clj Double/isNaN) x))

(defn log
  ([x]
   (#?(:clj Math/log :cljs js/Math.log) x))
  ([base x]
   (/ (log x) (log base))))

(defn exp
  "Exponential function. Returns b^n. If b not specified defaults to Euler's
  number."
  ([n]
   (#?(:clj Math/exp :cljs js/Math.exp) n))
  ([b n]
   (#?(:clj Math/pow :cljs js/Math.pow) b n)))

(defn dot [x y]
  (reduce + (map * x y)))

(defn norm [[x y]]
  (sqrt (+ (* x x) (* y y))))

(defn dist [[x1 y1] [x2 y2]]
  (norm [(- x1 x2) (- y1 y2)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Linear Algebra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mm [[a b c d] [x y]]
  [(+ (* a x) (* b y)) (+ (* c x) (* d y))])

(defn v+ [& vs]
  (apply mapv + vs))

(defn v- [& vs]
  (apply mapv - vs))

(defn v* [s [x y]]
  [(* s x) (* s y)])

(defn det [a b c d]
  (- (* a d) (* b c)))

(defn comp-atx
  "Returns the composition of affine transformations"
  ([] [1 0 0 1 0 0])
  ([a] a)
  ([[a b c d x y] [a' b' c' d' x' y']]
   [(+ (* a a') (* b c'))
    (+ (* a b') (* b d'))
    (+ (* c a') (* d c'))
    (+ (* c b') (* d d'))
    (+ x (* a x') (* b y'))
    (+ y (* c x') (* d y'))])
  ([a b & more] (reduce comp-atx (comp-atx a b) more)))

(defn apply-atx
  "Applies affine tx to a point and returns the result."
  [[a b c d e f] [x y]]
  [(+ (* a x) (* b y) e) (+ (* c x) (* d y) f)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bound-points
  "Returns the bottom-left and top-right corners of the smallest box containing
  all points."
  ;; TODO: Obvious candidate for slowness
  [points]
  (let [xmin (apply min (map first points))
        xmax (apply max (map first points))
        ymin (apply min (map second points))
        ymax (apply max (map second points))]
    [[xmin ymin] [xmax ymax]]))
