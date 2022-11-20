(ns cloud-descriptor.domain)

;; This namespace contains all records to represent the domain.
;; In this case, the domain is the cloud infrastructure.

(defrecord Attribute [name value])

(defrecord BlockAttribute [name attributes])

(defprotocol Resource
  (generate [resource] "Generates terraform code for given resource"))

(defrecord VPCResource [name attributes resources]
  Resource
  (generate [this] ""))

(defrecord SubnetResource [name attributes resources]
  Resource
  (generate [this] ""))

(defrecord EC2Resource [name attributes]
  Resource
  (generate [this] ""))
