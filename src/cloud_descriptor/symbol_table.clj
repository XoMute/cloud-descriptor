(ns cloud-descriptor.symbol-table
  (:require [cloud-descriptor.utils :refer :all]))

;; TODO: tests for symbol table

(def ^:dynamic *sym-tab*)
(def infradesc-sym-tab)
(def terraform-sym-tab)
(def id-counter)

(defrecord SymTabEntry [id owner entity]) ;; TODO: rename

(defn reset-generator!
  []
  (alter-var-root #'id-counter (constantly (atom 0))))

(defn initialize
  []
  (alter-var-root #'infradesc-sym-tab (constantly (atom {:entries ;; TODO: rename
                                                         []})))
  (alter-var-root #'terraform-sym-tab (constantly (atom {:entries []})))
  (reset-generator!))

(def generate-id
  (fn [] (do (swap! id-counter inc) @id-counter)))

(defmacro with-sym-tab
  "Bind infradesc symbol table to `*sym-tab*`"
  [& body]
  `(binding [*sym-tab* infradesc-sym-tab]
     (do ~@body)))

(defmacro with-terraform
  "Bind terraform symbol table to `*sym-tab*`"
  [& body]
  `(binding [*sym-tab* terraform-sym-tab]
     (do ~@body)))

(def node-attrs
  (comp :attributes :entity))

(def node-resources
  (comp :resources :entity))

(def node-name
  (comp :name :entity))

(def node-value
  (comp :value :entity))

(defn resource-to-sym-tab-entry
  [owner-id resource]
  ;; TODO: transform all owner ids to owner nodes to make lookup O(1)
  (let [id (generate-id)
        attributes (->> (:attributes resource)
                        (map #(resource-to-sym-tab-entry id %))
                        doall)
        resources (->> (:resources resource)
                       (map #(resource-to-sym-tab-entry id %))
                       doall)
        updated-resource (maybe-assoc-in resource [:attributes] attributes)
        updated-resource (maybe-assoc-in updated-resource [:resources] resources)]
    (->SymTabEntry id owner-id updated-resource)))

(defn new-node
  [owner-id entity]
  (let [id (generate-id)]
    (->SymTabEntry id owner-id entity)))

(defn resource-to-sym-tab!
  [resource owner-id]
  (let [entry (resource-to-sym-tab-entry owner-id resource)]
    (swap! *sym-tab* #(update % :entries conj entry))
    entry))

;; todo: symbol table should be filled during parsing
(defn fill-sym-tab!
  [& resources]
  (if (nil? @*sym-tab*)
    (throw (ex-info "Symbol table is uninitialized"
                    {}))
    (->> resources
         (map #(resource-to-sym-tab! % -1))  ;; TODO: figure something out with '-1'
         doall)))

(defn sym-tab-entities
  []
  (->> @*sym-tab*
       :entries
       (map :entity)))

(defn get-all-resource-nodes
  "Returns lazy list of all entries with given entity type located in *sym-tab*"
  [res-type]
  (letfn [(%helper [resource-node]
            (let [resource (:entity resource-node)
                  resources 
                  (->> resource
                       :resources
                       (mapcat %helper))]
              (if (instance? res-type resource)
                (cons resource-node resources)
                resources)))]

    (->> (:entries @*sym-tab*)
         (mapcat %helper))))

(defn get-all-resources
  [res-types]
  (->> (get-all-resource-nodes res-types)
       (map :entity)))

;; TODO: function to traverse symbol table tree

(defn get-node
  "TODO: improve speed"
  [node-id]
  (letfn [(%helper [node]
            (if (= node-id (:id node))
              node
              (let [attributes (node-attrs node)
                    resources (node-resources node)]
                (or (find-first %helper attributes)
                    (find-first %helper resources)))))]

    (let [entries (:entries @*sym-tab*)
          node (reduce (fn [acc item]
                         (or acc
                             (%helper item)))
                       nil
                       entries)]
      (when-not node
        (throw (ex-info "Can't find node with such id"
                        {:node-id node-id})))

      node)))

(defn node-owner
  [node]
  (let [owner-id (:owner node)]
    (or (and owner-id
             (>= owner-id 0)
             (get-node owner-id))
        (throw (ex-info "All nodes must have an owner"
                      {:node node
                       :owner-id owner-id})))))

(defn node-get-attr-node
  [node name]
  (->> (node-attrs node)
       (filter #(= (node-name %) name))
       last))

(defn entity-get-attr
  [entity name]
  (->> (:attributes entity)
       (map :entity)
       (filter #(= (:name %) name))
       last))

(defn node-get-attr
  [node name]
  (entity-get-attr (:entity node) name))

(defn entity-get-attr-val
  [entity name]
  (->> (entity-get-attr entity name)
       :value))

(defn node-get-attr-val
  [node name]
  (entity-get-attr-val (:entity node) name))

(defn replace-node
  [old new]
  (letfn [(%helper [node]
            (if (= (:id old)
                   (:id node))
              new
              (let [attributes (node-attrs node)
                    resources (node-resources node)
                    updated-attrs (map %helper attributes)
                    updated-resources (map %helper resources)]
                (-> node
                    (maybe-assoc-in [:entity :attributes] updated-attrs)
                    (maybe-assoc-in [:entity :resources] updated-resources)))))]

    (let [nodes (:entries @*sym-tab*)
          updated-nodes (mapv %helper nodes)]
      (assoc @*sym-tab* :entries updated-nodes))))

(defn update-node-entity! ;; TODO: rewrite
  [node entity]
  (->> entity
       (assoc node :entity)
       (replace-node node)
       (reset! *sym-tab*)))

(defn node-add-attr!
  "TODO: make more sophisticated"
  [node-id attr-node]
  (let [node (get-node node-id)
        new-node (update-in node [:entity :attributes] conj attr-node)]
    (reset! *sym-tab* (replace-node node new-node))
    new-node))

(defn node-remove-attr!
  [node-id attr-node]
  (let [node (get-node node-id)
        new-node (update-in node [:entity :attributes]
                            (fn [attrs]
                              (remove #(= (:id attr-node)
                                          (:id %))
                                      attrs)))]
    (reset! *sym-tab* (replace-node node new-node))
    new-node))

(defn post-traverse-sym-tab
  "1. Traverse all sym tab nodes.
   2. Apply `key-fn` to all nested resources and attributes.
   3. Update resources and attrs in node
   4. Apply `key-fn` to updated resource"
  [key-fn]
  (letfn [(%helper [resource-node]
            (when resource-node
              (let [resources (->> (node-resources resource-node)
                                   (map %helper))
                    attrs (->> (node-attrs resource-node)
                               (map %helper))
                    resource-node (maybe-assoc-in resource-node [:entity :resources]
                                                  resources)
                    resource-node (maybe-assoc-in resource-node [:entity :attributes]
                                                  attrs)]
                (key-fn resource-node))))]

    (->> (:entries @*sym-tab*)
         (map %helper))))

(defn nodes->entities ;; TODO: rename
  "Transform all sym tab nodes into entities"
  []
  (post-traverse-sym-tab #(:entity %)))

(defn tf-get-resource-nodes
  [res-type]
  (->> (get-all-resource-nodes Object)
       (filter #(= res-type (:type (:entity %))))))

(defn tf-get-resource-node
  [res-type]
  (first (tf-get-resource-nodes res-type)))
