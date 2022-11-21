(ns cloud-descriptor.symbol-table)

;; Symbol table contains:
;; - all resource names and types
;; - 

(def ^:dynamic *sym-tab*)
(def infradesc-sym-tab)
(def terraform-sym-tab)
(def id-counter)

(defrecord SymTabEntry [id entity])

(defn initialize
  []
  (alter-var-root #'infradesc-sym-tab (constantly (atom {:entries nil})))
  (alter-var-root #'terraform-sym-tab (constantly (atom {:entries nil})))
  (alter-var-root #'id-counter (constantly (atom 0))))

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

(defn resource-to-sym-tab
  [resource]
  (let [id (generate-id)
        entry (->SymTabEntry id resource)]
    (swap! *sym-tab* #(update-in %1 [:entries] conj entry))))

;; todo: symbol table should be filled during parsing
(defn fill-sym-tab
  [& resources]
  (if (nil? @*sym-tab*)
    (throw (ex-info "Symbol table is uninitialized"
                    {}))
    (doall
     (map resource-to-sym-tab resources))))
