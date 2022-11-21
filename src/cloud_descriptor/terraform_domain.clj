(ns cloud-descriptor.terraform-domain)

(defprotocol Generatable
  (generate [resource] "Generates terraform code for given resource"))

;; Implementation in `cloud-descriptor.generator`
(defrecord Attribute [name value])

(defrecord BlockAttribute [name attributes])

(defrecord Resource [name type attributes])
