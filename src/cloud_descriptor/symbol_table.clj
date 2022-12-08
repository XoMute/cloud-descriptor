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
  [resource]
  (let [entry (resource-to-sym-tab-entry -1 resource)] ;; TODO: figure something out with '-1'
    (swap! *sym-tab* #(update-in %1 [:entries] conj entry))))

;; todo: symbol table should be filled during parsing
(defn fill-sym-tab!
  [& resources]
  (if (nil? @*sym-tab*)
    (throw (ex-info "Symbol table is uninitialized"
                    {}))
    (doall
     (map resource-to-sym-tab! resources))))

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

(defn entity-get-attr-val  ;; TODO: DON'T TOUCH UNTILL ALL TESTS PASS!!!!
  [entity name]
  (->> (:attributes entity)
       (map :entity)
       (filter #(= (:name %) name))
       last
       :value))

(defn node-get-attr-val
  [node name]
  (entity-get-attr-val (:entity node) name))

(defn update-node!
  [node ;; TODO
   ]
  )

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
          updated-nodes (map %helper nodes)]
      (assoc @*sym-tab* :entries updated-nodes))))

(defn node-add-attr!
  "TODO: make more sophisticated"
  [node attr-node]
  (let [new-node (update-in node [:entity :attributes] conj attr-node)]
    (reset! *sym-tab* (replace-node node new-node))))

