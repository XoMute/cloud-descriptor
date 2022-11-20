(ns cloud-descriptor.symbol-table)

;; Symbol table contains:
;; - all resource names and types
;; - 
(def ^:dynamic *sym-tab*)

(defmacro with-sym-tab
  "Initialization of symbol table"
  [& body]
  (binding [cloud-descriptor.symbol-table/*sym-tab* {:resources nil}]
    (cons 'do body)))
