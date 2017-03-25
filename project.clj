(defproject subjure/subjure "0.0.1"
  :source-paths ["src"]
  :resource-paths ["resources"]
  :test-paths ["test"]
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/clojurescript "1.9.495"]]
  :plugins [[lein-cljsbuild "1.1.5"]]
  
  ;; An advanced compilation setup to check the muance bundle size
  :cljsbuild {:builds
              [{:source-paths ["src"]
                :compiler {:output-to "target/cljs/corale.min.js"
                           :optimizations :advanced
                           :pretty-print false}}]})
