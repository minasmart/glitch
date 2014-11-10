(defproject glitch "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2371"]
                 [squelch "0.1.2-SNAPSHOT"]
                 [rm-hull/monet "0.2.1"]]

  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
  :plugins [[lein-cljsbuild "1.0.4-SNAPSHOT"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "glitch"
              :source-paths ["src"]
              :compiler {
                :output-to "glitch.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
