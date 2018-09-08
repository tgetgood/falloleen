(ns falloleen.renderer.canvas
  (:require [clojure.string :as string]))

(defn- cmd
  "Converts idomatic kebab-case to upperCamelCase for JS commands"
  [sym]
  (let [chunks (string/split (name sym) #"-")]
    (symbol (apply str "." (first chunks) (map string/capitalize (rest chunks)))))
  )

(defmacro defcanvas
  ([n] `(defcanvas ~n []))
  ([n args]
   (let [tname (gensym)]
     `(do
        (deftype ~tname ~args
          falloleen.renderer.canvas/IExec
          (exec [_# ctx#]
            (~(cmd n) ctx# ~@args))

          ~'IPrintWithWriter
          (~'-pr-writer [o# w# _#]
           (~'write-all w# (str "#CanvasInstruction" o#)))

          ~'Object
          (~'toString [_#]
           (str "(" ~(str (cmd n)) " ctx" ~@(interleave (repeat " ") args) ")")))
        (def ~n
          ~(if (empty? args)
             `(new ~tname)
             `(fn ~args (new ~tname ~@args))))))))
