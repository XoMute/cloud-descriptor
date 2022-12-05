(ns cloud-descriptor.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [cloud-descriptor.file-io :refer :all]
            [cloud-descriptor.parser :refer :all]
            [cloud-descriptor.symbol-table :refer :all]
            [cloud-descriptor.translator :refer :all]
            [cloud-descriptor.generator :refer :all])
  (:import [java.lang Boolean])
  (:gen-class))

(def cli-options
  [[nil "--auto-generate-cidr-blocks BOOL" "Generate missing cidr blocks for VPC and Subnet resources"
    :id :auto-generate-cidr-blocks
    :default true
    :parse-fn #(Boolean/valueOf %)]
   ["-h" "--help"]])

(defn- usage
  [opts-summary]
  (->> ["Translator from InfraDesc to Terraform."
        ""
        "Usage: lein run [options] file"
        ""
        "Options:"
        opts-summary]
       (clojure.string/join \newline)))

(defn- error-msg
  [errors]
  (str "The following errors occured while executing the translator:\n\n"
       (clojure.string/join \newline errors)))

(defn- validate-args
  "Validate command line arguments"
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options)
      {:exit-message (usage summary) :ok? true}
      errors
      {:exit-message (error-msg errors)}

      ;; TODO: some custom arguments validation
      (= 1 (count arguments))
      {:file (first arguments) :options options}
      :else
      {:exit-message (usage summary)})))

(defn- exit
  [status message]
  (println message)
  (System/exit status))

(defn input->symbol-table ;; TODO: move
  [input]
  (let [parse-result (parse input)]
    (fill-sym-tab! parse-result)))

(defn- translate-infra ;; TODO: move
  [input options]
  (initialize)
  (with-sym-tab
    (input->symbol-table input)
    (translate-to-tf options))
  (with-terraform
    (generate-resources)))

(defn -main 
  [& args]
  (let [{:keys [file options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (let [out-path (get-out-dir! file)
            output (-> file
                       get-file-contents
                       (translate-infra options))]
        (write-to-main-in-dir! out-path output)
        (println (str "Translation is complete. Go to '" out-path "' and execute 'terraform plan' to see the generated infrastructure"))))))
