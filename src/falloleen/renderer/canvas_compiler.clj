(ns falloleen.renderer.canvas-compiler)

(defmacro instructions [& args]
  `(instructions* [~@args]))
