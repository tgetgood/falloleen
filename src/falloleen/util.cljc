(ns falloleen.util
  (:require [falloleen.math :as math]
            [net.cgrand.macrovich :as macros :include-macros true]))

(macros/deftime
  (defmacro implement-sequentials
    "Generates boilerplate to implement a protocol identically for all
     sequential things."
    {:style/indent [1 :form [1]]}
    [prot & methods]
    (let [types (macros/case :cljs '[List
                                     LazySeq
                                     PersistentVector
                                     IndexedSeq
                                     ChunkedSeq
                                     ArrayList]
                             :clj '[clojure.lang.PersistentVector
                                    clojure.lang.PersistentList
                                    clojure.lang.ArraySeq
                                    clojure.lang.IndexedSeq
                                    clojure.lang.PersistentVector$ChunkedSeq
                                    clojure.lang.LazySeq])]
      `(extend-protocol ~prot
         ~@(mapcat (fn [a b] `[~a ~@b]) types (repeat methods))))))

(defn magnitude
  "Returns the linear scale of the matrix [[a b][c d]]. Otherwise known as the
  square root of the absolute value of the determinant."
  [a b c d]
  ;; HACK: This works for symmetric linear transforms, but as soon as we start
  ;; talking about skews and asymmetric scalings, it breaks down. I don't see
  ;; any way to manage this without writing my own pixel shaders. Hopefully I do
  ;; before I do.
  ;; !!!!!!!
  ;; Unless I ditch paths altogether and use bezier curves --- actually pairs of
  ;; curves --- to represent the edges of objects. They have no stroke, just a
  ;; fill, and so I can control exactly how thick the line is at all points. Soo
  ;; much work... But it has the potential to be a solution.
  (let [m (math/sqrt (math/abs (math/det a b c d)))]
    (if (math/nan? m)
      1
      m)))
