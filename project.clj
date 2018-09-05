(defproject macroexpanse/falloleen "0.1.0-SNAPSHOT"
  :description "Lisp for vector graphics."
  :url "https://github.com/tgetgood/falloleen"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :scm {:name "git"
        :url  "https://github.com/tgetgood/falloleen"}

  :deploy-repositories [["releases" :clojars]]

  :min-lein-version "2.7.1"

  :dependencies [[net.cgrand/macrovich "0.2.1"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.238"]
                 [org.clojure/core.async  "0.4.474"]
                 [quil "2.7.1" :exclusions [[org.clojure/clojure]]]]

  :plugins [[lein-figwheel "0.5.16"]
            [lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]]

  :source-paths ["src"]

  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src"]
                :figwheel {:on-jsload "falloleen.core/on-js-reload"}

                :compiler {:main falloleen.core
                           :asset-path "js/compiled/out"
                           :output-to "resources/public/js/compiled/falloleen.js"
                           :output-dir "resources/public/js/compiled/out"
                           :source-map-timestamp true
                           :preloads [devtools.preload]}}
               {:id "min"
                :source-paths ["src"]
                :compiler {:output-to "resources/public/js/compiled/falloleen.js"
                           :main falloleen.core
                           :optimizations :advanced
                           :pretty-print false}}]}

  :profiles {:dev {:dependencies [[binaryage/devtools "0.9.9"]
                                  [figwheel-sidecar "0.5.16"]
                                  [cider/piggieback "0.3.1"]]
                   :source-paths ["src" "dev"]
                   :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}
                   :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                                     :target-path]}})
