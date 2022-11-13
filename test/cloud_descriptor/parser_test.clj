(ns cloud-descriptor.parser-test
  (:require [clojure.test :refer :all]
            [cloud-descriptor.core :refer :all]
            [cloud-descriptor.parser :refer :all]))

;;todo: a better way to test for exceptions
;; todo: split to nested tests
;; todo: string literal tests
(deftest parser-test
  (testing "Parsing"
    (testing "empty string"
      (is (thrown? clojure.lang.ExceptionInfo
                   (parse "")))
      (is (thrown? clojure.lang.ExceptionInfo
                   (parse "      "))
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
                 (parse input-string))
              "> VPC"))
        (let [input-string "Subnet TestSubnet"]
          (is (= [:S [:Resource
                      [:ResourceType "Subnet"]
                      [:Name "TestSubnet"]]]
                 (parse input-string))
              "> Subnet"))
        (let [input-string "EC2 TestEC2"]
          (is (= [:S [:Resource
                      [:ResourceType "EC2"]
                      [:Name "TestEC2"]]]
                 (parse input-string))
              "> EC2"))
        
        (let [input-string "Gateway TestGateway"]
          (is (thrown? Exception
                       (parse input-string))
              "> Unknown"))
        
        (testing "with wrong name"
          (let [input-string "EC2"]
            (is (thrown? clojure.lang.ExceptionInfo
                         (parse input-string))
                "> EC2 without a name"))
          (let [input-string "EC2 1abcd"]
            (is (thrown? clojure.lang.ExceptionInfo
                         (parse input-string))
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
                   (parse input-string))))))
      
      (testing "Subnet"
        (testing "with shorthand init"
          (let [input-string "Subnet TestSubnet = 10.0.0.0/24"]
            (is (= [:S
                    [:Resource
                     [:ResourceType "Subnet"]
                     [:Name "TestSubnet"]
                     [:ShorthandInit [:PlainString "10.0.0.0/24"]]]]
                   (parse input-string)))))

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
                   (parse input-string)))))))

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
                 (parse input-string))))))

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
                           [:Name "Subnet2"]]]]]]]]]]]
                 (parse input-string))))))))
