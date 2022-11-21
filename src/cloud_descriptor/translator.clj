(ns cloud-descriptor.translator
  (:require [cloud-descriptor.symbol-table :refer :all]
            [cloud-descriptor.domain :refer :all]))

(defn translate
  "Translate `*sym-tab*` into Terraform code"
  [] ;; todo: add translation options
  (println "sym tab" @*sym-tab*)
  ;; Do needed transformations
  ;; Generate resources in `*sym-tab*`
  (->> @*sym-tab*
      :entries
      (map :entity)
      (map generate)))
