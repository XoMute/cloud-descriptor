(ns cloud-descriptor.domain)

;; This namespace contains all records to represent the domain.
;; In this case, the domain is the cloud infrastructure.

(defprotocol Translatable
  (translate [resource] "Translate resource to Terraform resource"))

;; Implemenation in `cloud-descriptor.translator`
(defrecord Attribute [name value])

(defrecord BlockAttribute [name attributes])

(defrecord VPCResource [name attributes resources])

(defrecord SubnetResource [name attributes resources])

(defrecord EC2Resource [name attributes])
