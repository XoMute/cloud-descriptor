(ns cloud-descriptor.parser-test
  (:require [clojure.test :refer :all]
            [cloud-descriptor.core :refer :all]
            [cloud-descriptor.parser :refer :all]
            [cloud-descriptor.domain :refer :all]))

;; todo: string literal tests
(deftest parser-test
  (testing "Parsing"
    (testing "empty string"
      (is (thrown? clojure.lang.ExceptionInfo
                   (parse-to-tree "")))
      (is (thrown? clojure.lang.ExceptionInfo
                   (parse-to-tree "      "))
          "> with whitespaces"))
    (testing "array"
      (let [input-string "['123', '56', Subnet1,subnet2, [subnet3, \"abcd\\b\"]]"]
        (is (= [:Array
                [:String "123"]
                [:String "56"]
                [:Name "Subnet1"]
                [:Name "subnet2"]
                [:Array
                 [:Name "subnet3"]
                 [:String "abcd\\b"]]]
               (parser input-string :start :Array)))))
    (testing "single resource"
      (testing "without init"
        (let [input-string "VPC TestVpc"] ;; todo: test where vpc without init fails
          (is (= [:S [:Resource
                      [:ResourceType "VPC"]
                      [:Name "TestVpc"]]]
                 (parse-to-tree input-string))
              "> VPC"))
        (let [input-string "Subnet TestSubnet"]
          (is (= [:S [:Resource
                      [:ResourceType "Subnet"]
                      [:Name "TestSubnet"]]]
                 (parse-to-tree input-string))
              "> Subnet"))
        (let [input-string "EC2 TestEC2"]
          (is (= [:S [:Resource
                      [:ResourceType "EC2"]
                      [:Name "TestEC2"]]]
                 (parse-to-tree input-string))
              "> EC2"))
        
        (let [input-string "Gateway TestGateway"]
          (is (thrown? Exception
                       (parse-to-tree input-string))
              "> Unknown"))
        
        (testing "with wrong name"
          (let [input-string "EC2"]
            (is (thrown? clojure.lang.ExceptionInfo
                         (parse-to-tree input-string))
                "> EC2 without a name"))
          (let [input-string "EC2 1abcd"]
            (is (thrown? clojure.lang.ExceptionInfo
                         (parse-to-tree input-string))
                "> EC2 with name not starting from a letter"))))

      (testing "VPC"
        (testing "with block init"
          (let [input-string
                "VPC TestVPC {
                   cidr_block = 10.0.0.0/16
                   region = eu-north-1
                 }"]
            (is (= [:S
                    [:Resource
                     [:ResourceType "VPC"]
                     [:Name "TestVPC"]
                     [:Block
                      [:Attribute
                       [:Name "cidr_block"]
                       [:ShorthandInit
                        [:PlainString "10.0.0.0/16"]]]
                      [:Attribute
                       [:Name "region"]
                       [:ShorthandInit [:PlainString "eu-north-1"]]]]]]
                   (parse-to-tree input-string))))))
      
      (testing "Subnet"
        (testing "with shorthand init"
          (let [input-string "Subnet TestSubnet = 10.0.0.0/24"]
            (is (= [:S
                    [:Resource
                     [:ResourceType "Subnet"]
                     [:Name "TestSubnet"]
                     [:ShorthandInit [:PlainString "10.0.0.0/24"]]]]
                   (parse-to-tree input-string)))))

        (testing "with block init"
          (let [input-string
                "Subnet TestSubnet {
                   cidr_block = 10.0.0.0/24
                   availability_zone = zone-1
                 }"]
            (is (= [:S
                    [:Resource
                     [:ResourceType "Subnet"]
                     [:Name "TestSubnet"]
                     [:Block
                      [:Attribute
                       [:Name "cidr_block"]
                       [:ShorthandInit [:PlainString "10.0.0.0/24"]]]
                      [:Attribute
                       [:Name "availability_zone"]
                       [:ShorthandInit [:PlainString "zone-1"]]]]]]
                   (parse-to-tree input-string)))))))

    (testing "basic infrastructure:"
      (testing "VPC with one subnet with one EC2"
        (let [input-string
              "VPC TestVPC {
                     cidr_block = 10.0.0.0/16
                     region = eu-north-1

                     Subnet TestSubnet {
                       cidr_block = 10.0.0.0/24

                       EC2 TestEC2 {
                         ami = \"ami-128371923\"
                         instance_type = t3.micro

                         networking {
                           ingress = all
                         }
                       }
                     }
                   }"]
          (is (= [:S
                  [:Resource
                   [:ResourceType "VPC"]
                   [:Name "TestVPC"]
                   [:Block
                    [:Attribute
                     [:Name "cidr_block"]
                     [:ShorthandInit [:PlainString "10.0.0.0/16"]]]
                    [:Attribute
                     [:Name "region"]
                     [:ShorthandInit [:PlainString "eu-north-1"]]]
                    [:Resource
                     [:ResourceType "Subnet"]
                     [:Name "TestSubnet"]
                     [:Block
                      [:Attribute
                       [:Name "cidr_block"]
                       [:ShorthandInit [:PlainString "10.0.0.0/24"]]]
                      [:Resource
                       [:ResourceType "EC2"]
                       [:Name "TestEC2"]
                       [:Block
                        [:Attribute
                         [:Name "ami"]
                         [:ShorthandInit [:String "ami-128371923"]]]
                        [:Attribute
                         [:Name "instance_type"]
                         [:ShorthandInit [:PlainString "t3.micro"]]]
                        [:Attribute
                         [:Name "networking"]
                         [:Block
                          [:Attribute
                           [:Name "ingress"]
                           [:ShorthandInit [:PlainString "all"]]]]]]]]]]]]
                 (parse-to-tree input-string))))))

    (testing "complex infrastructure:"
      (testing "VPC with two subnets and autoscaling EC2"
        (let [input-string
              "VPC TestVPC {
                     cidr_block = 10.0.0.0/16
                     region = eu-north-1

                     Subnet TestSubnet1 = 10.0.11.0/24
                     Subnet TestSubnet2

                     EC2 TestEC2 {
                       name = example-rest-service
                       instance_type = t3.micro
                       image = ubuntu
                       autoscaling {
                         min_size = 1
                         max_size = 4
                         desired_capacity = 1
                         subnets = [Subnet1, Subnet2]
                       }

                     networking {
                       ingress = [http, ssh]
                       egress = all
                     }
                     }
                   }"]
          (is (= [:S
                  [:Resource
                   [:ResourceType "VPC"]
                   [:Name "TestVPC"]
                   [:Block
                    [:Attribute
                     [:Name "cidr_block"]
                     [:ShorthandInit [:PlainString "10.0.0.0/16"]]]
                    [:Attribute
                     [:Name "region"]
                     [:ShorthandInit [:PlainString "eu-north-1"]]]
                    [:Resource
                     [:ResourceType "Subnet"]
                     [:Name "TestSubnet1"]
                     [:ShorthandInit [:PlainString "10.0.11.0/24"]]]
                    [:Resource
                     [:ResourceType "Subnet"]
                     [:Name "TestSubnet2"]]
                    [:Resource
                     [:ResourceType "EC2"]
                     [:Name "TestEC2"]
                     [:Block
                      [:Attribute
                       [:Name "name"]
                       [:ShorthandInit [:PlainString "example-rest-service"]]]
                      [:Attribute
                       [:Name "instance_type"]
                       [:ShorthandInit [:PlainString "t3.micro"]]]
                      [:Attribute
                       [:Name "image"]
                       [:ShorthandInit [:PlainString "ubuntu"]]]
                      [:Attribute
                       [:Name "autoscaling"]
                       [:Block
                        [:Attribute
                         [:Name "min_size"]
                         [:ShorthandInit [:PlainString "1"]]]
                        [:Attribute
                         [:Name "max_size"]
                         [:ShorthandInit [:PlainString "4"]]]
                        [:Attribute
                         [:Name "desired_capacity"]
                         [:ShorthandInit [:PlainString "1"]]]
                        [:Attribute
                         [:Name "subnets"]
                         [:ShorthandInit
                          [:Array
                           [:Name "Subnet1"]
                           [:Name "Subnet2"]]]]]]
                      [:Attribute
                       [:Name "networking"]
                       [:Block
                        [:Attribute
                         [:Name "ingress"]
                         [:ShorthandInit
                          [:Array
                           [:Name "http"]
                           [:Name "ssh"]]]]
                        [:Attribute
                         [:Name "egress"]
                         [:ShorthandInit
                          [:PlainString "all"]]]]]]]]]]
                 (parse-to-tree input-string))))))))

(deftest transform-tree-test
  (testing "Transforming parse tree:"
    (testing "Single resource"
      (testing "VPC"
        (testing "empty VPC"
          (let [parse-tree [:Resource
                            [:ResourceType "VPC"]
                            [:Name "TestVPC"]]]
            (is (thrown? Exception (transform-tree parse-tree)))))

        (testing "shorthand init should throw error"
          (let [parse-tree [:Resource
                            [:ResourceType "VPC"]
                            [:Name "TestVPC"]
                            [:ShorthandInit [:PlainString "10.0.0.0/0"]]]]
            (is (thrown? Exception (transform-tree parse-tree)))))

        (testing "block init"
          (let [parse-tree [:Resource
                            [:ResourceType "VPC"]
                            [:Name "TestVPC"]
                            [:Block
                             [:Attribute
                              [:Name "some-attr"]
                              [:ShorthandInit
                               [:PlainString "some-val"]]]]]
                transformed (transform-tree parse-tree)]
            (is (= (->VPCResource "TestVPC"
                                  [(->Attribute "some-attr" "some-val")]
                                  [])
                   transformed))))

        (testing "with different attribute order"
          (let [parse-tree-1 [:Resource
                              [:ResourceType "VPC"]
                              [:Block
                               [:Attribute
                                [:Name "some-attr"]
                                [:ShorthandInit
                                 [:PlainString "10.0.0.0/16"]]]]
                              [:Name "TestVPC"]]
                parse-tree-2 [:Resource
                              [:Block
                               [:Attribute
                                [:Name "some-attr"]
                                [:ShorthandInit
                                 [:PlainString "10.0.0.0/16"]]]]
                              [:Name "TestVPC"]
                              [:ResourceType "VPC"]]
                transformed-1 (transform-tree parse-tree-1)
                transformed-2 (transform-tree parse-tree-2)]
            (is (= transformed-1
                   transformed-2))))))
    (testing "Subnet"
      (testing "without init"
        (let [parse-tree [:Resource
                          [:ResourceType "Subnet"]
                          [:Name "TestSubnet"]]
              transformed (transform-tree parse-tree)]
          (is (= (->SubnetResource "TestSubnet" [] [])
                 transformed))))

      (testing "with shorthand init"
        (let [parse-tree [:Resource
                          [:ResourceType "Subnet"]
                          [:Name "TestSubnet"]
                          [:ShorthandInit
                           [:PlainString "10.0.0.0/24"]]]
              transformed (transform-tree parse-tree)]
          (is (= (->SubnetResource "TestSubnet"
                                   [(->Attribute "cidr_block" "10.0.0.0/24")]
                                   [])
                 transformed))))

      (testing "with block init"
        (let [parse-tree [:Resource
                          [:ResourceType "Subnet"]
                          [:Name "TestSubnet"]
                          [:Block
                           [:Attribute
                            [:Name "some-attr"]
                            [:ShorthandInit
                             [:PlainString "some value"]]]]]
              transformed (transform-tree parse-tree)]
          (is (= (->SubnetResource "TestSubnet"
                                   [(->Attribute "some-attr" "some value")]
                                   [])
                 transformed)))))

    (testing "EC2"
      (testing "without init"
        (let [parse-tree [:Resource
                          [:ResourceType "EC2"]
                          [:Name "TestServer"]]]
          (is (thrown? Exception (transform-tree parse-tree)))))

      (testing "with shorthand init"
        (let [parse-tree [:Resource
                          [:ResourceType "EC2"]
                          [:Name "TestServer"]
                          [:ShorthandInit
                           [:PlainString "some value"]]]]
          (is (thrown? Exception (transform-tree parse-tree)))))

      (testing "with block init"
        (let [parse-tree [:Resource
                          [:ResourceType "EC2"]
                          [:Name "TestServer"]
                          [:Block
                           [:Attribute
                            [:Name "some-attr"]
                            [:ShorthandInit
                             [:PlainString "some value"]]]]]
              transformed (transform-tree parse-tree)]
          (is (= (->EC2Resource "TestServer"
                                [(->Attribute "some-attr" "some value")])
                 transformed)))

        (testing "with block attribute"
          (let [parse-tree [:Resource
                            [:ResourceType "EC2"]
                            [:Name "TestServer"]
                            [:Block
                             [:Attribute
                              [:Name "some-attr"]
                              [:ShorthandInit
                               [:PlainString "some value"]]]
                             [:Attribute
                              [:Name "networking"]
                              [:Block
                               [:Attribute
                                [:Name "some-nested-attr"]
                                [:ShorthandInit
                                 [:String "some value"]]]]]]]
                transformed (transform-tree parse-tree)]
            (is (= (->EC2Resource "TestServer"
                                  [(->Attribute "some-attr" "some value")
                                   (->BlockAttribute "networking"
                                                     [(->Attribute "some-nested-attr" "some value")])])
                   transformed))))))

    (testing "Unknown resource type"
      (let [parse-tree [:Resource
                        [:ResourceType "EC22"]
                        [:Name "TestServer"]]]
        (is (thrown? Exception (transform-tree parse-tree)))))))

;; todo: test with unsupported block attribute for ec2
;; todo: test with uninitialized attributes
(deftest full-parsing-test
  (testing "Parsing"
    (testing "VPC with two subnets"
      (let [input-string "VPC TestVPC { cidr_block = 172.16.0.0/16 Subnet TestSubnet = 172.16.4.0/24 }"
            parse-result (parse input-string)]
        (is (= (->VPCResource "TestVPC"
                              [(->Attribute "cidr_block" "172.16.0.0/16")]
                              [(->SubnetResource "TestSubnet"
                                                 [(->Attribute "cidr_block" "172.16.4.0/24")]
                                                 [])])
               parse-result))))

    (testing "basic infrastructure"
      (let [input-string
            "VPC TestVPC {
                   cidr_block = 10.0.0.0/16
                   region = eu-north-1

                   Subnet TestSubnet {
                     cidr_block = 10.0.0.0/24

                     EC2 TestEC2 {
                       ami = \"ami-128371923\"
                       instance_type = t3.micro

                       networking {
                         ingress = all
                       }
                     }
                   }
                 }"
            parse-result (parse input-string)]
        (is (= (->VPCResource
                "TestVPC"
                [(->Attribute "cidr_block" "10.0.0.0/16")
                 (->Attribute "region" "eu-north-1")]
                [(->SubnetResource
                  "TestSubnet"
                  [(->Attribute "cidr_block" "10.0.0.0/24")]
                  [(->EC2Resource
                    "TestEC2"
                    [(->Attribute "ami" "ami-128371923")
                     (->Attribute "instance_type" "t3.micro")
                     (->BlockAttribute
                      "networking"
                      [(->Attribute "ingress" "all")])])])])
               parse-result))))

    (testing "complex infrastructure"
      (let [input-string
            "VPC TestVPC {
                   cidr_block = 10.0.0.0/16
                   region = eu-north-1

                   Subnet TestSubnet1 = 10.0.11.0/24
                   Subnet TestSubnet2

                   EC2 TestEC2 {
                     name = example-rest-service
                     instance_type = t3.micro
                     image = ubuntu
                     autoscaling {
                       min_size = 1
                       max_size = 4
                       desired_capacity = 1
                       subnets = [Subnet1, Subnet2]
                     }

                     networking {
                       ingress = [http, ssh]
                       egress = all
                     }
                   }
                 }"
            parse-result (parse input-string)]
        (is (= (->VPCResource
                "TestVPC"
                [(->Attribute "cidr_block" "10.0.0.0/16")
                 (->Attribute "region" "eu-north-1")]
                [(->SubnetResource
                  "TestSubnet1"
                  [(->Attribute "cidr_block" "10.0.11.0/24")]
                  [])
                 (->SubnetResource "TestSubnet2" [] [])
                 (->EC2Resource
                  "TestEC2"
                  [(->Attribute "name" "example-rest-service")
                   (->Attribute "instance_type" "t3.micro")
                   (->Attribute "image" "ubuntu")
                   (->BlockAttribute
                    "autoscaling"
                    [(->Attribute "min_size" "1")
                     (->Attribute "max_size" "4")
                     (->Attribute "desired_capacity" "1")
                     (->Attribute "subnets" ["Subnet1", "Subnet2"])])
                   (->BlockAttribute
                    "networking"
                    [(->Attribute "ingress" ["http", "ssh"])
                     (->Attribute "egress" "all")])])])
               parse-result))))))
