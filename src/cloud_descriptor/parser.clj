(ns cloud-descriptor.parser
  (:require [instaparse.core :as insta]))

(def parser ;; todo: make private
  (insta/parser (clojure.java.io/resource "grammar.bnf")))

;; todo: rename
(defn parse
  [string]
  (let [result (parser string)]
    (if (insta/failure? result)
      (throw (ex-info "The parsing failed"
                      (insta/get-failure result)))
      (insta/transform {:ResourceName (fn [& chars] [:ResourceName (apply str chars)])}
                       result))))


;; todo: use insta/transform
