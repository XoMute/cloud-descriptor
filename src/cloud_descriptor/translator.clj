(ns cloud-descriptor.translator
  (:require [cloud-descriptor.symbol-table :refer :all]
            [cloud-descriptor.domain :as d :refer [translate]]
            [cloud-descriptor.terraform-domain :as tfd]
            [cloud-descriptor.utils :refer :all]
            [cloud-descriptor.transformations.auto-cidr-blocks :refer :all])
  (:import [cloud_descriptor.domain Attribute
            BlockAttribute
            VPCResource
            SubnetResource
            EC2Resource]))

(declare translate-to-list)

(def ^:dynamic *owner-id*) ;; tfd/QualifiedName

;; Code in this file translates InfraDesc symbol table to Terraform symbol table

(defn- find-attribute ;; TODO: write in disser that in case of duplicate attributes no error will be thrown
  [attributes name]
  (->> attributes
       (filter #(= (:name %) name))
       last))

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
        image (find-attribute attributes "image")
        instance-type (find-attribute attributes "instance_type")]
    (when-not (or ami image)
      (throw (ex-info "EC2 resources must contain 'ami' attribute"
                      {})))

    (when-not instance-type
      (throw (ex-info "EC2 resources must contain 'instance_type' attribute"
                      {})))
    attributes))

(defn generate-owner-attribute
  [attr-name]
  (when *owner-id*
    (tfd/->Attribute attr-name *owner-id*)))

(extend-protocol d/Translatable
  Attribute
  (translate [this]
    ;; TODO: array value
    (tfd/->Attribute (:name this) (:value this)))

  BlockAttribute
  (translate [this]
    (tfd/->BlockAttribute (:name this)
                          (->> (:attributes this)
                               (map :entity)
                               (map translate))))

  VPCResource
  (translate [this]
    (binding [*owner-id* (tfd/->QualifiedName "aws_vpc"
                                              (:name this)
                                              "id")]
      (let [attributes (->> (:attributes this)
                            (map :entity)
                            (validate-vpc-attributes)
                            (map translate))
            resources (->> (:resources this)
                           (map :entity)
                           (validate-vpc-resources)
                           translate-to-list)
            [provider-attrs vpc-attrs]
            (->> attributes
                 (split-by #(= (:name %) "region")))
            provider (tfd/->Provider "aws" provider-attrs)
            vpc (tfd/->Resource (:name this) "aws_vpc"
                                 vpc-attrs)]
        (concat [provider vpc]
                resources))))

  SubnetResource
  (translate [this]
    (let [owner-attr (generate-owner-attribute "vpc_id")]
      (binding [*owner-id* (tfd/->QualifiedName "aws_subnet"
                                                (:name this)
                                                "id")]
        (let [attributes (->> (:attributes this)
                              (map :entity)
                              (map translate))
              resources (->> (:resources this)
                             (map :entity)
                             (validate-subnet-resources)
                             translate-to-list)]
          (concat [(tfd/->Resource (:name this) "aws_subnet"
                                   (cons owner-attr attributes))]
                  resources)))))

  EC2Resource
  (translate [this]
    ;; todo: generate network interface for ec2
    (let [owner-attr (generate-owner-attribute "subnet_id")
          attributes (->> (:attributes this)
                          (map :entity)
                          (validate-ec2-attributes)
                          (map translate))]
      (tfd/->Resource (:name this) "aws_instance"
                      (cons owner-attr attributes)))))

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
  (->> (sym-tab-entities)
       translate-to-list
       doall))

(defn translate-sym-tab
  "Translate `*sym-tab*` into Terraform `*sym-tab*`"
  [& {:keys [auto-generate-cidr-blocks]
      :or {auto-generate-cidr-blocks nil}}] ;; todo: add translation options
  ;; Run needed transformations
  (when auto-generate-cidr-blocks
    (auto-generate-cidr-blocks!))
  ;; TODO: transformation to add network interfaces to ec2 instances
  ;; TODO: transformation to add lifecycle?
  ;; Translate resources in `*sym-tab*`
  (translate-resources))

(defn translate-to-tf
  [options]
  (->> (into [] cat options)
       (apply translate-sym-tab)
       ;; todo: make correct entries
       (reset! terraform-sym-tab)))

