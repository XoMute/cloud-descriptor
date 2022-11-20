(ns cloud-descriptor.parser
  (:require [instaparse.core :as insta]
            [cloud-descriptor.domain :refer :all]
            [cloud-descriptor.utils :refer :all]))

;; todo: check block attributes names in EC2

(declare transform-tree transform-expr-tree)

(def parser ;; todo: make private
  (insta/parser (clojure.java.io/resource "grammar.bnf")))

(defn- find-attributes
  [tree]
  (->> tree
       (filter #(= (first %) :Attribute))
       (map transform-tree)))

(defn- find-resources
  [tree]
  (->> tree
       (filter #(= (first %) :Resource))
       (map transform-tree)))

(defn transform-vpc
  [res-name block shorthand]
  (cond
    (seq block)
    (let [attributes (find-attributes block)
          resources (find-resources block)]
      (->VPCResource res-name attributes resources))
    (seq shorthand)
    (throw (ex-info "Shorthand initialization for VPC resource is not allowed"
                    {:resource-name res-name}))
    :else
    (throw (ex-info "VPC can't be empty"
                    {:resource-name res-name}))))

(defn transform-subnet
  [res-name block shorthand]
  (cond
    (seq block)
    (let [attributes (find-attributes block)
          resources (find-resources block)]
      (->SubnetResource res-name attributes resources))
    (seq shorthand)
    (->SubnetResource res-name
                      [(->Attribute "cidr_block" (transform-expr-tree shorthand))]
                      [])
    :else
    (->SubnetResource res-name [] [])))

(defn transform-ec2
  [res-name block shorthand]
  (cond
    (seq block)
    (let [attributes (find-attributes block)]
      (->EC2Resource res-name attributes))
    (seq shorthand)
    (throw (ex-info "Shorthand initialization for EC2 resource is not allowed"
                    {:resource-name res-name}))
    :else
    (throw (ex-info "EC2 can't be empty"
                    {:resource-name res-name}))))

(defn transform-attribute
  [res-name block shorthand]
  (cond
    (seq block)
    (let [attributes (find-attributes block)]
      (->BlockAttribute res-name attributes))
    (seq shorthand)
    (->Attribute res-name (transform-expr-tree shorthand))
    :else
    (throw (ex-info "An attribute should be initialized"
                    {:attribute-name res-name}))))

(defn transform-expr-tree
  [parse-tree]
  (when parse-tree
    (let [type (first parse-tree)]
      (case type
        :String
        (second parse-tree) ;; todo: character escaping?

        :PlainString
        (second parse-tree) ;; todo: character escaping?

        :Array
        (mapv transform-expr-tree (rest parse-tree))

        :Name
        (second parse-tree) ;; todo: use ids

        (throw (ex-info "Can't transform parse tree"
                        {:parse-tree parse-tree
                         :unknown-element (first parse-tree)}))))))

(defn transform-tree
  [parse-tree]
  (when parse-tree
    (let [type (first parse-tree)
          res-name (second (find-direct-child parse-tree :Name))
          res-type (second (find-direct-child parse-tree :ResourceType))
          transform-fn ;; (fn [res-name block shorthand])
          (case type
            :Resource
            (if (nil? res-type)
              (throw (ex-info "The resource should have a type"
                              {:resource-name res-name
                               :resource-type res-type}))

              (case res-type
                "VPC" #'transform-vpc
                "Subnet" #'transform-subnet
                "EC2" #'transform-ec2
                (throw (ex-info "Unknown resource type"
                                {:resource-name res-name
                                 :resource-type res-type}))))

            :Attribute
            (let [res-name (second (find-direct-child parse-tree :Name))
                  block (rest (find-direct-child parse-tree :Block))
                  shorthand (second (find-direct-child parse-tree :ShorthandInit))]
              #'transform-attribute)

            (throw (ex-info "Can't transform parse tree"
                            {:parse-tree parse-tree
                             :unknown-element (first parse-tree)})))]

      (let [block (rest (find-direct-child parse-tree :Block))
            shorthand (second (find-direct-child parse-tree :ShorthandInit))]
        (if (nil? res-name)
          (throw (ex-info "The resource should have a name"
                          {:resource-name res-name
                           :resource-type res-type}))
          (transform-fn res-name block shorthand))))))

(defn parse-to-tree
  [input]
  (let [result (parser input)]
    (if (insta/failure? result)
      (throw (ex-info "The parsing failed"
                      (insta/get-failure result)))
      result)))

(defn get-root-resource
  [parse-tree]
  (if (not= 1 (count (rest parse-tree)))
    (throw (ex-info "More than 1 root resource is currently unsupported"
                    {}))
    (second parse-tree)))

(defn parse
  "Transforms input into objects"
  [input]
  (-> input
      parse-to-tree
      get-root-resource
      transform-tree))

