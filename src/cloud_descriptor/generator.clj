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
  (let [attributes (->> (:attributes resource)
                        (sort #(< (:id %1)
                                  (:id %2))) ;; TODO: maybe remove?
                        (map :entity)
                        (map generate))
        spaces (generate-spaces)]
    (if (seq attributes)
      (str \newline spaces
           (clojure.string/join (str \newline spaces) attributes)
           \newline)
      "")))

(extend-protocol Generatable
  Attribute
  (generate [this]
    (letfn [(generate-value [value]
              (cond
                (string? value)
                (str \" value \")
                (sequential? value)
                (map generate-value value)
                :else (generate value)))]

      (let [value (generate-value (:value this))]
        (if (sequential? value)
          (str (:name this) " = [" (clojure.string/join ", " value) "]")
          (str (:name this) " = " value)))))

  BlockAttribute
  (generate [this]
    (let [nested-attrs (indent
                        (generate-attributes this))]
      (str (:name this) " {"
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
           (:name this) "\" {"
           attrs
           (generate-spaces)
           "}")))

  Provider
  (generate [this]
    (let [region (entity-get-attr-val this "region")
          provider (str "provider " \" (:name this) \")
          attrs (indent
                 (generate-attributes this))
          block (str "{"
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
           (:name this) "\" {"
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
  (->> (sym-tab-entities)
       (map generate)
       (clojure.string/join "\n\n")))

