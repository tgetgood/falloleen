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
