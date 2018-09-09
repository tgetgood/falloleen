(ns falloleen.renderer.canvas
  (:require [clojure.string :as string]))

(defn- cmd-name
  "Converts idomatic kebab-case to upperCamelCase for JS commands"
  [sym]
  (let [chunks (string/split (name sym) #"-")]
    (symbol (apply str "." (first chunks) (map string/capitalize (rest chunks))))))

(defn- type-case
  "Converts idomatic kebab-case to UpperCamelCase for type names."
  [sym]
  (symbol (apply str (map string/capitalize (string/split (name sym) #"-")))))

(defmacro defstyle [n args]
  (let [tname (type-case n)
        k (keyword (name n))]
    `(do
       (deftype ~n ~args
         falloleen.renderer.canvas/IExec
         (exec [this ctx# stack#]
           (when (free? stack# ~k)
             (~(cmd-name n) ctx# ~@args)
             (assoc-style stack# ~k this)))

          ~'IPrintWithWriter
          (~'-pr-writer [o# w# _#]
           (~'write-all w# (str "#StyleInstruction" o#)))))))

(defmacro defcanvas
  ([n] `(defcanvas ~n []))
  ([n args]
   (let [tname (type-case n)]
     `(do
        (deftype ~tname ~args
          falloleen.renderer.canvas/IExec
          (exec [_# ctx# states#]
            (~(cmd-name n) ctx# ~@args)
            states#)

          ~'IPrintWithWriter
          (~'-pr-writer [o# w# _#]
           (~'write-all w# (str "#CanvasInstruction" o#)))

          ~'Object
          (~'toString [_#]
           (str "(" ~(str (cmd-name n)) " ctx" ~@(interleave (repeat " ") args) ")")))
        (def ~n
          ~(if (empty? args)
             `(new ~tname)
             `(fn ~args (new ~tname ~@args))))))))
