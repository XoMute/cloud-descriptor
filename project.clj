(defproject cloud-descriptor "0.1.0-SNAPSHOT"
  :description "Master's project for high-level cloud automation"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [instaparse "1.4.12"]
                 [org.clojure/tools.cli "1.0.214"]]
  :main ^:skip-aot cloud-descriptor.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
