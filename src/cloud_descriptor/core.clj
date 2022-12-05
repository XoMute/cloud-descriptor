(ns cloud-descriptor.core
  (:require [cloud-descriptor.file-io :refer :all]
            [cloud-descriptor.parser :refer :all]
            [cloud-descriptor.symbol-table :refer :all]
            [cloud-descriptor.translator :refer :all]
            [cloud-descriptor.generator :refer :all])
  (:gen-class))

(def help-message "Usage: lein run <path-to-input-file> [options]")

(defn input->symbol-table
  [input]
  (let [parse-result (parse input)]
    (fill-sym-tab! parse-result)))

(defn- translate-infra
  [input]
  (initialize)
  (with-sym-tab
    (input->symbol-table input)
    (translate-to-tf))
  (with-terraform
    (generate-resources)))

(defn -main 
  [& [path & options]]
  (if path
    (let [out-path (get-out-dir! path)
          output (-> path
                     get-file-contents
                     (translate-infra))]
      (write-to-main-in-dir! out-path output)
      (println (str "Translation is complete. Go to '" out-path "' and execute 'terraform plan' to see the generated infrastructure")))
    (println help-message)))
