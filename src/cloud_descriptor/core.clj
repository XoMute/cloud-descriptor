(ns cloud-descriptor.core
  (:require [instaparse.core :as insta]
            [cloud-descriptor.parser :refer :all]
            [cloud-descriptor.symbol-table :refer :all])
  (:gen-class))

(defn input->symbol-table
  [input]
  (let [parse-result (parse input)]
    (fill-sym-tab parse-result)))

(defn -main 
  [& args]
  (println "Hello, World!"))
