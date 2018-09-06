(ns falloleen.core
  (:require [clojure.string :as string]
            [falloleen.lang :as lang]
            [net.cgrand.macrovich :as macros :include-macros true])
  #?(:cljs (:require-macros [falloleen.core :refer [deftemplate]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translate
  [shape v]
  (lang/stack-transform shape (lang/translation v)))

(defn reflect
  ([shape axis]
   (lang/stack-transform shape (lang/reflection axis)))
  ([shape centre axis]
   (lang/transform-with-centre shape centre (lang/reflection axis))))

(defn scale
  ([shape extent]
   (lang/stack-transform shape (lang/scaling extent)))
  ([shape centre extent]
   (lang/transform-with-centre shape centre (lang/scaling extent))))

(defn rotate
  ([shape angle]
   (lang/stack-transform shape (lang/rotation angle)))
  ([shape centre angle]
   (lang/transform-with-centre shape centre (lang/rotation angle))))

(defn transform
  [shape xform]
  (lang/stack-transform shape (lang/atx xform)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- type-case
  "Converts idomatic kebab-case to UpperCamelCase for type names."
  [sym]
  (symbol (apply str (map string/capitalize (string/split (name sym) #"-")))))

(macros/deftime
  (defmacro deftemplate
    "Defines a new shape template. Creates a new record whose name is
  instance-name converted to UpperCamelCase as per record naming conventions.

  The canonical instance of the new template will be bound to instance-name. The
  canonical instance is the expansion of the default template arguments.

  Expansion will be executed in an environment when all keys of the template
  name have been bound to symbols. Expansions must return a valid shape
  (template or otherwise).

  Optionally impls are protocol implementations as per defrecord."
    {:style/indent [1 :form [1]]}
    [instance-name template expansion & impls]
    (let [template-name (type-case instance-name)
          fields (map (comp symbol name) (keys template))]
      `(do
         ;; TODO: I can generate a spec from the field list and then check it's
         ;; valid at expansion time. I think that would be a good place to find
         ;; errors.
         ;;
         ;; The problem is that adding a spec/def into this macro expansion
         ;; causes the whole thing to go haywire even though the relevant parts
         ;; of the expansion don't change at all...
         (defrecord ~template-name [~@fields]
           falloleen.lang/ITemplate
           (falloleen.lang/expand-template [this#]
             ~expansion)
           ~@impls)
         (def ~instance-name
           (~(symbol (str "map->" template-name)) ~template))))))

(defn ^boolean template? [shape]
  (satisfies? lang/ITemplate shape))

(defn template-expand-all [shape]
  (if (template? shape)
    (recur (lang/expand-template shape))
    shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Curves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Compositing Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Text Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
