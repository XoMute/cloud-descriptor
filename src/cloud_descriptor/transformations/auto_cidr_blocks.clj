(ns cloud-descriptor.transformations.auto-cidr-blocks
  (:require [cloud-descriptor.symbol-table :refer :all]
            [cloud-descriptor.domain :refer :all]
            [cloud-descriptor.utils :refer :all])
  (:import [cloud_descriptor.domain VPCResource SubnetResource]))

(defn- get-third-ip-number
  [cidr-block]
  (-> cidr-block
      (clojure.string/split #"\.")
      (nth 2)
      (Integer/parseInt)))

(defn- increase-network-mask
  [cidr-block]
  (let [splitted (clojure.string/split cidr-block #"/")
        mask (-> splitted
                 (nth 1)
                 (Integer/parseInt))]
    (when (not= 16 mask)
      (throw (ex-info "Network masks other than 16 are not yet supported for VPC"
                      {:cidr-block cidr-block
                       :mask mask})))

    (->> splitted
         (replace-nth 1 (+ mask 8))
         (clojure.string/join "/"))))

(defn- get-next-available-cidr-block
  ;; TODO: improve complexity of this function
  [node cidr-blocks]
  (if (seq cidr-blocks)
    (let [third-numbers (map get-third-ip-number cidr-blocks)
          biggest-number (->> third-numbers
                              (sort >)
                              first
                              inc)]
      (when (> biggest-number 255)
        (throw (ex-info "Can't auto-generate cidr block"
                        {:cidr-blocks cidr-blocks})))
      (->> (clojure.string/split (first cidr-blocks) #"\.")
           (replace-nth 2 biggest-number)
           (clojure.string/join ".")))

    (let [owner (node-owner node)
          cidr-block (node-get-attr-val owner "cidr_block")]
      (when-not cidr-block
        (throw (ex-info "Can't find cidr block of the owner"
                        {:node node
                         :owner owner})))
      (recur node
             [(increase-network-mask cidr-block)]))))

(defn auto-generate-cidr-blocks-for-vpc!
  []
  (let [[vpc-node :as vpc-nodes] (get-all-resource-nodes VPCResource)]
    (when (not= 1 (count vpc-nodes))
      (throw (ex-info "The infrastructure can currently contain only exactly 1 VPC resource"
                      {:vpc-resources (count vpc-nodes)})))
    
    (when (not (node-get-attr-val vpc-node "cidr_block"))
      (let [cidr-block "10.0.0.0/16"]
        (node-add-attr! vpc-node
                        (new-node (:id vpc-node)
                                  (->Attribute "cidr_block" cidr-block)))))))

(defn auto-generate-cidr-blocks-for-subnets!
  []
  (let [subnet-nodes (get-all-resource-nodes SubnetResource) 
        subnets (map :entity subnet-nodes)
        used-cidr-blocks (->> subnet-nodes
                              (map #(node-get-attr-val % "cidr_block"))
                              (remove nil?))
        subnet-nodes-without-cidr-block
        (filter
         #(not (node-get-attr-val % "cidr_block"))
         subnet-nodes)]
    (loop [subnet-nodes subnet-nodes-without-cidr-block
           cidr-blocks used-cidr-blocks]
      (when (seq subnet-nodes)
        (let [subnet-node (first subnet-nodes)
              cidr-block (get-next-available-cidr-block subnet-node
                                                        cidr-blocks)]
          (node-add-attr! subnet-node
                          (new-node (:id subnet-node)
                                    (->Attribute "cidr_block" cidr-block)))
          (recur (rest subnet-nodes)
                 (cons cidr-block cidr-blocks)))))))

(defn auto-generate-cidr-blocks!
  []
  (auto-generate-cidr-blocks-for-vpc!)
  (auto-generate-cidr-blocks-for-subnets!))

