(ns falloleen.hosts.jfx
  (:import [javafx.application Application Platform]
           [javafx.scene Group Scene]
           [javafx.scene.canvas Canvas GraphicsContext]
           javafx.stage.Stage))

(defn draw! [^GraphicsContext gc]
  (println gc))

(defn test-run []
  (Application/launch falloleen.hosts.jfx.application (make-array String 0)))

(defn kill []
  (Platform/exit))
