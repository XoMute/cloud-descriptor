(ns cloud-descriptor.symbol-table)

;; Symbol table contains:
;; - all resource names and types
;; - 
(def ^:dynamic *sym-tab*)

(defrecord SymTabEntry [id entity])

(def generate-id
  (let [id-counter (atom 0)]
    (fn [] (do (swap! id-counter inc) @id-counter))))

(defmacro with-sym-tab
  "Initialization of symbol table"
  [& body]
  `(binding [*sym-tab* (atom {:entries nil})]
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
