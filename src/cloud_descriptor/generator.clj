(ns cloud-descriptor.generator
  (:require [cloud-descriptor.symbol-table :refer :all]
            [cloud-descriptor.terraform-domain :refer :all])
  (:import [cloud_descriptor.terraform_domain
            Attribute
            BlockAttribute
            Resource
            Provider
            Data
            QualifiedName]))

(def ^:dynamic *indent* 0)
(def ^:const indent-size 2) ;; todo: move to options

(defmacro indent
  [& body]
  `(binding [*indent* (+ *indent* 2)]
     ~@body))

(defn generate-spaces
  []
  (->> (repeat *indent* \space)
       (apply str)))

(defn generate-attributes
  [resource]
  (let [attributes (map generate (:attributes resource))
        spaces (generate-spaces)]
    (str spaces
         (clojure.string/join (str \newline spaces) attributes)
         \newline)))

(extend-protocol Generatable
  Attribute ;; TODO: attribute value might also be an array
  (generate [this]
    (let [value (:value this)
          value (if (string? value)
                  (str \" value \")
                  (generate value))]
      (str (:name this) " = " value)))

  BlockAttribute
  (generate [this]
    (let [nested-attrs (indent
                        (generate-attributes this))]
      (str (:name this) " {\n"
           nested-attrs
           (generate-spaces)
           "}")))

  Resource
  (generate [this]
    (let [attrs (indent
                 (generate-attributes this))]
      (str (generate-spaces)
           "resource \""
           (:type this) "\" \""
           (:name this) "\" {\n"
           attrs
           (generate-spaces)
           "}")))

  Provider
  (generate [this]
    (let [region (->> (:attributes this)
                      (filter #(= (:name %) "region"))
                      first)
          provider (str "provider " \" (:name this) \")
          attrs (indent
                 (generate-attributes this))
          block (str "{\n"
                     attrs
                     (generate-spaces)
                     "}")]

      (str (generate-spaces) provider " " block)))

  Data
  (generate [this]
    (let [attrs (indent
                 (generate-attributes this))]
      (str (generate-spaces)
           "data \""
           (:type this) "\" \""
           (:name this) "\" {\n"
           attrs
           (generate-spaces)
           "}")))

  QualifiedName
  (generate [this]
    (str (:type this) "." (:name this) "." (:attribute this))))

(defn generate-terraform-resource
  [res-type res-name block]
  (str "resource \"" res-type "\" \"" res-name "\" " block))

(defn generate-resources
  "Traverse all resources in symbol table and generate them"
  []
  (->> @*sym-tab*
       (map generate)
       (clojure.string/join "\n\n")))

