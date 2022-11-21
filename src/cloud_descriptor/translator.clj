(ns cloud-descriptor.translator
  (:require [cloud-descriptor.symbol-table :refer :all]
            [cloud-descriptor.domain :as d :refer [translate]]
            [cloud-descriptor.terraform-domain :as tfd]
            [cloud-descriptor.utils :refer :all])
  (:import [cloud_descriptor.domain Attribute
                                    BlockAttribute
                                    VPCResource
                                    SubnetResource
                                    EC2Resource]))

(declare translate-to-list)

;; Code in this file translates InfraDesc symbol table to Terraform symbol table
(defn validate-vpc-attributes ;; todo: auto-generate cidr block (maybe do it as a separate translation step?)
  [attributes]
  (let [region (find-attribute attributes "region")]
    (when-not region
      (throw (ex-info "VPC resources must contain 'region' attribute"
                      {}))) ;; todo: add some context
    attributes))

(let [allowed-types #{SubnetResource EC2Resource}]
  (defn validate-vpc-resources
    [resources]
    (when-not (every? #(allowed-types (class %)) resources)
      (throw (ex-info "VPC resources can contain only Subnet or EC2 nested resources"
                      {}))) ;; todo: add some context
    resources))

(let [allowed-types #{EC2Resource}]
  (defn validate-subnet-resources
    [resources]
    (when-not (every? #(allowed-types (class %)) resources)
      (throw (ex-info "Subnet resources can contain only EC2 nested resources"
                      {}))) ;; todo: add some context
    resources))

(defn validate-ec2-attributes
  [attributes]
  (let [ami (find-attribute attributes "ami")
        instance-type (find-attribute attributes "instance_type")]
    (when-not ami
      (throw (ex-info "EC2 resources must contain 'ami' attribute"
                      {})))

    (when-not instance-type
      (throw (ex-info "EC2 resources must contain 'instance_type' attribute"
                      {})))
    attributes))

(extend-protocol d/Translatable
  Attribute
  (translate [this]
    (tfd/->Attribute (:name this) (:value this)))

  BlockAttribute
  (translate [this]
    (tfd/->BlockAttribute (:name this)
                          (->> (:attributes this)
                               (map translate))))

  VPCResource
  (translate [this]
    (let [attributes (->> (:attributes this)
                          (validate-vpc-attributes)
                          (map translate))
          resources (->> (:resources this)
                         (validate-vpc-resources)
                         translate-to-list)]
      (concat [(tfd/->Resource (:name this) "aws_vpc"
                               attributes)]
              resources)))

  SubnetResource
  (translate [this]
    (let [attributes (->> (:attributes this)
                          (map translate))
          resources (->> (:resources this)
                         (validate-subnet-resources)
                         translate-to-list)]
      (concat [(tfd/->Resource (:name this) "aws_subnet"
                               attributes)]
              resources)))

  EC2Resource
  (translate [this]
    (let [attributes (->> (:attributes this)
                          (validate-ec2-attributes)
                          (map translate))]
      (tfd/->Resource (:name this) "aws_instance"
                      attributes))))

(defn translate-to-list
  "Translates given entities and returns resulting resources in a single list"
  [entities]
  (->> entities
       (map translate)
       (map #(if (seq? %) % (list %)))
       (apply concat)))

(defn translate-resources
  "Traverse all resources in symbol table and translate them"
  []
  (->> @*sym-tab*
       :entries
       (map :entity)
       translate-to-list
       doall))

(defn translate-sym-tab
  "Translate `*sym-tab*` into Terraform `*sym-tab*`"
  [] ;; todo: add translation options
  ;; Do needed transformations
  ;; Translate resources in `*sym-tab*`
  (translate-resources))

