(defproject macroexpanse/falloleen "0.1.0-SNAPSHOT"
  :description "High level declarative graphics"
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
                 [quil "2.7.1" :exclusions [[org.clojure/clojure]]]]

  :source-paths ["src"])
