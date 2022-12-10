(ns cloud-descriptor.transformations.networking
  (:require [cloud-descriptor.symbol-table :refer :all]
            [cloud-descriptor.terraform-domain :refer :all]
            [cloud-descriptor.utils :refer :all])
  (:import [cloud_descriptor.terraform_domain Resource]))

(def ^:dynamic *ec2-name*)
(def ^:dynamic *ip*)

(defn- generate-tags
  [suffix]
  (->Attribute "tags" {"Name" (str *ec2-name* "-" suffix)}))

(defn- next-private-ip
  [subnet-node]
  ;; TODO: fix for separate ips in each subnet (e.g. hashmap with subnet ids)
  (let [cidr-block (node-value (node-get-attr-node subnet-node "cidr_block"))
        next-ip (swap! *ip* inc)]
    (if (> next-ip 255)
      (throw (ex-info ("Can't automatically generate ip address for ec2 instance with name: " *ec2-name*)
                      {}))
      (let [ip-address-parts (-> (clojure.string/split cidr-block #"/")
                                 first
                                 (clojure.string/split #"\."))]
        (->> ip-address-parts
             (replace-nth 3 next-ip)
             (clojure.string/join "."))))))

(defn- generate-network-interface!
  [sg-node]
  (let [subnet-node (tf-get-resource-node "aws_subnet") ;; TODO: it's not correct and won't work with multiple subnets
        subnet-id-attr (->Attribute "subnet_id" (->QualifiedName "aws_subnet"
                                                                 (node-name subnet-node)
                                                                 "id"))
        private-ips-attr (->Attribute "private_ips" [(next-private-ip subnet-node)])
        security-groups-attr (->Attribute "security_groups"
                                          [(->QualifiedName "aws_security_group"
                                                            (node-name sg-node)
                                                            "id")])
        network-interface-attrs [subnet-id-attr private-ips-attr security-groups-attr
                                 (generate-tags "ni")]
        network-interface (->Resource *ec2-name* "aws_network_interface" network-interface-attrs)]
    (resource-to-sym-tab! network-interface -1)))

(defn- attach-network-interface-to-ec2!
  [ec2-node network-interface-node]
  (let [network-interface-attrs [(->Attribute "device_index" "0")
                                 (->Attribute "network_interface_id"
                                              (->QualifiedName "aws_network_interface"
                                                               (node-name network-interface-node)
                                                               "id"))]
        network-interface-attr (->BlockAttribute "network_interface" network-interface-attrs)]
    (node-add-attr! (:id ec2-node)
                    (resource-to-sym-tab-entry (:id ec2-node) network-interface-attr))))

(defn- rule-name->rule-attrs
  [rule-name]
  (case rule-name
    "all" [(->Attribute "protocol" "-1")
           (->Attribute "from_port" "0")
           (->Attribute "to_port" "0")
           (->Attribute "cidr_blocks" ["0.0.0.0/0"])]
    "ssh" [(->Attribute "protocol" "tcp")
           (->Attribute "from_port" "22")
           (->Attribute "to_port" "22")
           (->Attribute "cidr_blocks" ["0.0.0.0/0"])]
    "http" [(->Attribute "protocol" "tcp")
            (->Attribute "from_port" "80")
            (->Attribute "to_port" "80")
            (->Attribute "cidr_blocks" ["0.0.0.0/0"])]
    "https" [(->Attribute "protocol" "tcp")
             (->Attribute "from_port" "443")
             (->Attribute "to_port" "443")
             (->Attribute "cidr_blocks" ["0.0.0.0/0"])]
    (throw (ex-info (str "Uknown rule name: " rule-name)
                    {:rule-name rule-name}))))

(defn- generate-security-group-rules
  [networking-attr-node]
  (letfn [(generate-security-group-rule [attr]
            (let [val (node-value attr)]
              (if (sequential? val)
                (map #(->BlockAttribute (node-name attr)
                                        (rule-name->rule-attrs %))
                     val)
                [(->BlockAttribute (node-name attr)
                                   (rule-name->rule-attrs (node-value attr)))])))]

    (->> (node-attrs networking-attr-node)
         (mapcat generate-security-group-rule))))

(defn- generate-security-group!
  [networking-attr-node]
  (let [vpc-node (tf-get-resource-node "aws_vpc")
        rules (generate-security-group-rules networking-attr-node)
        security-group-attrs (list* (->Attribute "vpc_id"
                                             (->QualifiedName "aws_vpc" (node-name vpc-node) "id"))
                                    (generate-tags "sg")
                                    rules)
        security-group (->Resource *ec2-name* "aws_security_group" security-group-attrs)]
    (resource-to-sym-tab! security-group -1)))

(defn- generate-eip!
  []
  ;; TODO: fix naming of resource - it won't work if called multiple times
  (let [eip-attrs [(->Attribute "vpc" "true")
                   (generate-tags "eip")]
        eip (->Resource *ec2-name* "aws_eip" eip-attrs)]
    (resource-to-sym-tab! eip -1)))

(defn- generate-nat!
  [networking-attr-node]
  (let [subnet-node (tf-get-resource-node "aws_subnet") ;; TODO: it's not correct and won't work with multiple subnets
        eip-node (generate-eip!)
        nat-attrs [(->Attribute "allocation_id"
                                (->QualifiedName "aws_eip" (node-name eip-node) "id"))
                   (->Attribute "subnet_id"
                                (->QualifiedName "aws_subnet" (node-name subnet-node) "id"))
                   (generate-tags "nat")]
        nat (->Resource *ec2-name* "aws_nat_gateway" nat-attrs)]
    (resource-to-sym-tab! nat -1)))

;; TODO: split this func
(defn- generate-route-table!
  [networking-attr-node nat-node sg-node]
  (let [vpc-node (tf-get-resource-node "aws_vpc")
        subnet-node (tf-get-resource-node "aws_subnet") ;; TODO: it's not correct and won't work with multiple subnets
        egress-cidr-blocks (->> (node-attrs sg-node)
                                (filter #(= "egress"
                                            (node-name %)))
                                (mapcat node-attrs)
                                (filter #(= "cidr_blocks"
                                            (node-name %)))
                                (mapcat node-value))
        routes (map #(->BlockAttribute "route"
                                       [(->Attribute "cidr_block" %)
                                        (->Attribute "nat_gateway_id"
                                                     (->QualifiedName "aws_nat_gateway"
                                                                      (node-name nat-node)
                                                                      "id"))])
                    egress-cidr-blocks)
        route-table-attrs (list* (->Attribute "vpc_id" (->QualifiedName "aws_vpc" (node-name vpc-node) "id"))
                                 (generate-tags "rt")
                                 routes)
        route-table (->Resource *ec2-name* "aws_route_table" route-table-attrs)

        route-table-association-attrs [(->Attribute "subnet_id" (->QualifiedName "aws_subnet"
                                                                                 (node-name subnet-node)
                                                                                 "id"))
                                       (->Attribute "route_table_id" (->QualifiedName "aws_route_table"
                                                                                      (:name route-table)
                                                                                      "id"))]
        route-table-association (->Resource *ec2-name* "aws_route_table_association" route-table-association-attrs)]
    [(resource-to-sym-tab! route-table -1)
     (resource-to-sym-tab! route-table-association -1)]))

(defn- generate-internet-gateway!
  []
  (let [vpc-node (tf-get-resource-node "aws_vpc")
        attrs [(->Attribute "vpc_id" (->QualifiedName "aws_vpc" (node-name vpc-node) "id"))
               (generate-tags "igw")]
        internet-gateway (->Resource *ec2-name* "aws_internet_gateway" attrs)]
    (resource-to-sym-tab! internet-gateway -1)))

(defn- generate-public-eip! ;; TODO: merge with `generate-ip!`
  [network-interface-node internet-gateway-node]
  (let [ec2-private-ips (node-get-attr-val network-interface-node "private_ips")
        eip-attrs [(->Attribute "vpc" "true")
                   (->Attribute "network_interface"
                                (->QualifiedName "aws_network_interface" (node-name network-interface-node) "id"))
                   (->Attribute "associate_with_private_ip" (first ec2-private-ips)) ;; TODO: support for multiple private ips
                   (generate-tags "eip")]
        eip (->Resource *ec2-name* "aws_eip" eip-attrs)]
    (resource-to-sym-tab! eip -1)))

(defn- process-networking-attr!
  [ec2-node]
  (binding [*ec2-name* (node-name ec2-node)
            *ip* (atom 3)]
    (let [networking-attr-node (node-get-attr-node ec2-node "networking")
          has-ingress? (some #(= "ingress" (node-name %))
                             (node-attrs networking-attr-node))
          has-egress? (some #(= "egress" (node-name %))
                            (node-attrs networking-attr-node))]

      ;; actions to do in all cases
      (let [security-group-node (generate-security-group! networking-attr-node)
            network-interface-node (generate-network-interface! security-group-node)
            ec2-node (attach-network-interface-to-ec2! ec2-node network-interface-node)]

        (cond ;; TODO: improve clarity
          (and has-ingress?
               has-egress?)
          ;; no need to generate NAT if the ec2 instance has a public IP address
          (let [internet-gateway-node (generate-internet-gateway!)]
            (generate-public-eip! network-interface-node internet-gateway-node))
          has-ingress?
          ;; actions to do when networking contains at least one ingress rule
          (let [internet-gateway-node (generate-internet-gateway!)]
            (generate-public-eip! network-interface-node internet-gateway-node))
          has-egress?
          ;; actions to do when networking contains at least one egress rule
          (let [nat-node (generate-nat! networking-attr-node)
                [route-table-node route-table-association-node]
                (generate-route-table! networking-attr-node
                                       nat-node
                                       security-group-node)]))

        ;; remove networking attr
        (let [ec2-node (node-remove-attr! (:id ec2-node) networking-attr-node)
              ec2-node (node-remove-attr! (:id ec2-node) (node-get-attr-node ec2-node "subnet_id")) ;; TODO: move from here
              ]
          )))))

(defn networking-transformation!
  []
  (->> (tf-get-resource-nodes "aws_instance")
       (map process-networking-attr!)
       dorun))
