(ns cloud-descriptor.translator-test
  (:require [clojure.test :refer :all]
            [cloud-descriptor.core :refer :all]
            [cloud-descriptor.parser :refer :all]
            [cloud-descriptor.symbol-table :refer :all]
            [cloud-descriptor.translator :refer :all]
            [cloud-descriptor.terraform-domain :refer :all]))

;; todo: test for auto-generation of attributes and resources

(defn- translate-full
  []
  (translate-sym-tab :auto-generate-cidr-blocks true))

(deftest translator-test
  (testing "Translation of"
    (testing "single VPC"
      (testing "empty should throw error"
        (let [input-string "VPC TestVPC {}"]
          (initialize)
          (with-sym-tab
            (is (thrown? clojure.lang.ExceptionInfo (input->symbol-table input-string))))))

      (testing "with attributes\n"
        (testing "> without required attributes"
          (let [input-string "VPC TestVPC {cidr_block=10.0.0.0/16}"]
            (initialize)
            (with-sym-tab
              (input->symbol-table input-string)
              (is (thrown? clojure.lang.ExceptionInfo (translate-sym-tab))))))

        (testing "> with required attributes"
          (let [input-string "VPC TestVPC {cidr_block=10.0.0.0/16 region=eu-north-1}"]
            (initialize)
            (with-sym-tab
              (input->symbol-table input-string)
              (is (= [(->Resource "TestVPC" "aws_vpc"
                                  [(->Attribute "cidr_block" "10.0.0.0/16")
                                   (->Attribute "region" "eu-north-1")])]
                     (translate-sym-tab))))))
        (testing "> with auto-generated cidr_block"
          (let [input-string "VPC TestVPC { region=eu-north-1 }"]
            (initialize)
            (with-sym-tab
              (input->symbol-table input-string)
              (is (= [(->Resource "TestVPC" "aws_vpc"
                                  [(->Attribute "cidr_block" "10.0.0.0/16")
                                   (->Attribute "region" "eu-north-1")])]
                     (translate-sym-tab :auto-generate-cidr-blocks :true)))))))

      (testing "with one subnet"
        (let [input-string "VPC TestVPC {
  cidr_block=10.0.0.0/16
  region=eu-north-1
  Subnet TestSubnet = 10.0.5.0/24
}"]
          (initialize)
          (with-sym-tab
            (input->symbol-table input-string)
            (is (= [(->Resource "TestVPC" "aws_vpc"
                                [(->Attribute "cidr_block" "10.0.0.0/16")
                                 (->Attribute "region" "eu-north-1")])
                    (->Resource "TestSubnet" "aws_subnet"
                                [(->Attribute "vpc_id"
                                              (->QualifiedName "aws_vpc" "TestVPC" "id"))
                                 (->Attribute "cidr_block" "10.0.5.0/24")])]
                   (translate-sym-tab)))))

        (testing "with auto-generated cidr_block"
          (testing "auto-generate cidr_block"
            (let [input-string "VPC TestVPC {
  cidr_block=10.0.0.0/16
  region=eu-north-1
  Subnet TestSubnet
}"]
              (initialize)
              (with-sym-tab
                (input->symbol-table input-string)
                (is (= [(->Resource "TestVPC" "aws_vpc"
                                    [(->Attribute "cidr_block" "10.0.0.0/16")
                                     (->Attribute "region" "eu-north-1")])
                        (->Resource "TestSubnet" "aws_subnet"
                                    [(->Attribute "vpc_id"
                                                  (->QualifiedName "aws_vpc"
                                                                   "TestVPC"
                                                                   "id"))
                                     (->Attribute "cidr_block" "10.0.1.0/24")])]
                       (translate-sym-tab :auto-generate-cidr-blocks :true))
                    "generate cidr_block for subnet")))
          
            (let [input-string "VPC TestVPC {
  region=eu-north-1
  Subnet TestSubnet
}"]
              (initialize)
              (with-sym-tab
                (input->symbol-table input-string)
                (is (= [(->Resource "TestVPC" "aws_vpc"
                                    [(->Attribute "cidr_block" "10.0.0.0/16")
                                     (->Attribute "region" "eu-north-1")])
                        (->Resource "TestSubnet" "aws_subnet"
                                    [(->Attribute "vpc_id"
                                                  (->QualifiedName "aws_vpc"
                                                                   "TestVPC"
                                                                   "id"))
                                     (->Attribute "cidr_block" "10.0.1.0/24")])]
                       (translate-sym-tab :auto-generate-cidr-blocks :true)))))))
        (testing "and empty EC2"
          (let [input-string "VPC TestVPC {
  cidr_block=10.0.0.0/16
  region=eu-north-1
  Subnet TestSubnet {
    cidr_block = 10.0.6.0/24
    availability_zone = eu-north-1a
    EC2 TestServer {
    }
  }
}"]
            (initialize)
            (with-sym-tab
              (is (thrown? clojure.lang.ExceptionInfo
                           (input->symbol-table input-string))))))

        (testing "and EC2\n"
          (testing "> without required attributes"
            (let [input-string "VPC TestVPC {
  cidr_block=10.0.0.0/16
  region=eu-north-1
  Subnet TestSubnet {
    cidr_block = 10.0.6.0/24
    availability_zone = eu-north-1a
    EC2 TestServer {
      ami = ubuntu
    }
  }
}"]
              (initialize)
              (with-sym-tab
                (input->symbol-table input-string)
                (is (thrown? clojure.lang.ExceptionInfo (translate-sym-tab))))))

          (testing "> with required attributes"
            (let [input-string "VPC TestVPC {
  cidr_block=10.0.0.0/16
  region=eu-north-1
  Subnet TestSubnet {
    cidr_block = 10.0.6.0/24
    availability_zone = eu-north-1a
    EC2 TestServer {
      ami = ubuntu
      instance_type = t3.micro
    }
  }
}"]
              (initialize)
              (with-sym-tab
                (input->symbol-table input-string)
                (is (= [(->Resource "TestVPC" "aws_vpc"
                                    [(->Attribute "cidr_block" "10.0.0.0/16")
                                     (->Attribute "region" "eu-north-1")])
                        (->Resource "TestSubnet" "aws_subnet"
                                    [(->Attribute "vpc_id"
                                                  (->QualifiedName "aws_vpc"
                                                                   "TestVPC"
                                                                   "id"))
                                     (->Attribute "cidr_block" "10.0.6.0/24")
                                     (->Attribute "availability_zone" "eu-north-1a")])
                        (->Resource "TestServer" "aws_instance"
                                    [(->Attribute "subnet_id"
                                                  (->QualifiedName "aws_subnet"
                                                                   "TestSubnet"
                                                                   "id"))
                                     (->Attribute "ami" "ubuntu")
                                     (->Attribute "instance_type" "t3.micro")])]
                       (translate-sym-tab))))))))

      (testing "with multiple subnets:"
        (testing "auto-generate cidr_blocks"
          (let [input-string "VPC TestVPC {
  cidr_block=10.0.0.0/16
  region=eu-north-1
  Subnet TestSubnet1
  Subnet TestSubnet2
}"]
            (initialize)
            (with-sym-tab
              (input->symbol-table input-string)
              (is (= [(->Resource "TestVPC" "aws_vpc"
                                  [(->Attribute "cidr_block" "10.0.0.0/16")
                                   (->Attribute "region" "eu-north-1")])
                      (->Resource "TestSubnet1" "aws_subnet"
                                  [(->Attribute "vpc_id"
                                                (->QualifiedName "aws_vpc"
                                                                 "TestVPC"
                                                                 "id"))
                                   (->Attribute "cidr_block" "10.0.1.0/24")])
                      (->Resource "TestSubnet2" "aws_subnet"
                                  [(->Attribute "vpc_id"
                                                (->QualifiedName "aws_vpc"
                                                                 "TestVPC"
                                                                 "id"))
                                   (->Attribute "cidr_block" "10.0.2.0/24")])]
                     (translate-sym-tab :auto-generate-cidr-blocks :true)))))

          (let [input-string "VPC TestVPC {
  cidr_block=10.0.0.0/16
  region=eu-north-1
  Subnet TestSubnet1 = 10.12.4.0/24
  Subnet TestSubnet2
}"]
            (initialize)
            (with-sym-tab
              (input->symbol-table input-string)
              (is (= [(->Resource "TestVPC" "aws_vpc"
                                  [(->Attribute "cidr_block" "10.0.0.0/16")
                                   (->Attribute "region" "eu-north-1")])
                      (->Resource "TestSubnet1" "aws_subnet"
                                  [(->Attribute "vpc_id"
                                                (->QualifiedName "aws_vpc"
                                                                 "TestVPC"
                                                                 "id"))
                                   (->Attribute "cidr_block" "10.12.4.0/24")])
                      (->Resource "TestSubnet2" "aws_subnet"
                                  [(->Attribute "vpc_id"
                                                (->QualifiedName "aws_vpc"
                                                                 "TestVPC"
                                                                 "id"))
                                   (->Attribute "cidr_block" "10.12.5.0/24")])]
                     (translate-sym-tab :auto-generate-cidr-blocks :true))
                  "generate cidr_block for subnet"))))

        (testing "generate subnets in different availability zones"
          ;; TODO
          ))

      (testing "with EC2" ;; without subnets
        ;; TODO
        ))

    (testing "basic infrastructure"
      (let [input-string "VPC ExampleVPC {
  cidr_block = 172.22.0.0/16
  region = eu-north-1

  Subnet ExampleSubnet {

    EC2 ExampleEC2 {
      ami = \"ami-078e13ebe3b027f1c\"
      instance_type = \"t3.micro\"

      networking {
        ingress = all
      }
    }
  }
}"]
        (initialize)
        (with-sym-tab
          (input->symbol-table input-string)
          (is (= [(->Resource "ExampleVPC" "aws_vpc"
                              [(->Attribute "cidr_block" "172.22.0.0/16")
                               (->Attribute "region" "eu-north-1") ;; TODO: move region to provider
                               ])
                  (->Resource "ExampleSubnet" "aws_subnet"
                              [(->Attribute "vpc_id"
                                            (->QualifiedName "aws_vpc"
                                                             "ExampleVPC"
                                                             "id"))
                               (->Attribute "cidr_block" "172.22.1.0/24")])
                  (->Resource "ExampleEC2" "aws_instance"
                              [(->Attribute "subnet_id"
                                            (->QualifiedName "aws_subnet"
                                                             "ExampleSubnet"
                                                             "id"))
                               (->Attribute "ami" "ami-078e13ebe3b027f1c")
                               (->Attribute "instance_type" "t3.micro")
                               (->BlockAttribute "networking"
                                                 [(->Attribute "ingress" "all")])])]
                 (translate-full))))))

    (testing "autoscaling block"
      (testing "reference to incorrect subnet name"
        ;; TODO
        )
      (testing "using cidr block instead of existing subnet"
        ;; TODO
        ))

    ;; TODO: replace 'image' with auto lookup
    #_(testing "complex infrastructure"
        (let [input-string "VPC MainVPC {
  cidr_block = 10.0.0.0/16
  region = eu-north-1

  Subnet Subnet1 = 10.0.11.0/24
  Subnet Subnet2 = 10.0.12.0/24

  EC2 RestService {
    name = \"example-rest-service\"
    instance_type = \"t3.micro\"
    image = \"ubuntu\"

    autoscaling {
      min_size = 1
      max_size = 2
      desired_capacity = 1
      subnets = [Subnet1, Subnet2]
    }

    networking {
      ingress = http
      egress = all
    }
  }
}"]
          (initialize)
          (with-sym-tab
            (input->symbol-table input-string)
            (is (= [(->Resource "MainVPC" "aws_vpc"
                                [(->Attribute "cidr_block" "10.0.0.0/16")
                                 (->Attribute "region" "eu-north-1") ;; TODO: move region to provider
                                 ])
                    (->Resource "Subnet1" "aws_subnet"
                                [(->Attribute "vpc_id"
                                              (->QualifiedName "aws_vpc" "MainVPC" "id"))
                                 (->Attribute "cidr_block" "10.0.11.0/24")
                                 ;; TODO: availability_zone
                                 ])
                    (->Resource "Subnet2" "aws_subnet"
                                [(->Attribute "vpc_id"
                                              (->QualifiedName "aws_vpc" "MainVPC" "id"))
                                 (->Attribute "cidr_block" "10.0.12.0/24")
                                 ;; TODO: availability_zone
                                 ])
                    (->Resource "InternetGateway" "aws_internet_gateway"
                                [(->Attribute "vpc_id"
                                              (->QualifiedName "aws_vpc" "MainVPC" "id"))])
                    (->Resource "RestService" "aws_launch_configuration"
                                [(->Attribute "name_prefix" "TODO")
                                 (->Attribute "image_id" "TODO")
                                 (->Attribute "instance_type" "t3.micro")
                                 (->Attribute "security_groups" "TODO: array value [aws_security_group.RestServiceInstance.id]")
                                 (->BlockAttribute "lifecycle"
                                                   [(->Attribute "create_before_destroy" "true")])])
                    (->Resource "RestService" "aws_autoscaling_group"
                                [(->Attribute "name" "TODO")
                                 (->Attribute "min_size" "1")
                                 (->Attribute "max_size" "2")
                                 (->Attribute "desired_capacity" "1")
                                 (->Attribute "launch_configuration"
                                              (->QualifiedName "aws_launch_configuration"
                                                               "RestService"
                                                               "name"))
                                 (->Attribute "vpc_zone_identifier" "TODO: array value [aws_subnet.Subnet1.id, aws_subnet.Subnet2.id]")])

                    (->Resource "RestService" "aws_lb"
                                [(->Attribute "name" "TODO")
                                 (->Attribute "internal" "false") ;; TODO: bool
                                 (->Attribute "load_balancer_type" "application")
                                 (->Attribute "security_groups" "TODO: array value [aws_security_group.RestServiceLoadBalancer.id]")
                                 (->Attribute "subnets" "TODO: array value [aws_subnet.Subnet1.id, aws_subnet.Subnet2.id]")])
                    (->Resource "RestService" "aws_lb_listener"
                                [(->Attribute "load_balancer_arn"
                                              (->QualifiedName "aws_lb" "RestService" "arn"))
                                 (->Attribute "port" "80")
                                 (->Attribute "protocol" "HTTP")
                                 (->BlockAttribute "default_action"
                                                   [(->Attribute "type" "forward")
                                                    (->Attribute "target_group_arn"
                                                                 (->QualifiedName "aws_lb_target_group" "RestService" "arn"))])])
                    (->Resource "RestService" "aws_lb_target_group"
                                [(->Attribute "vpc_id"
                                              (->QualifiedName "aws_vpc" "MainVPC" "id"))
                                 (->Attribute "name" "TODO")
                                 (->Attribute "port" "80")
                                 (->Attribute "protocol" "HTTP")])
                    (->Resource "RestService" "aws_autoscaling_attachment"
                                [(->Attribute "autoscaling_group_name"
                                              (->QualifiedName "aws_autoscaling_group" "RestService" "id"))
                                 (->Attribute "lb_target_group_arn"
                                              (->QualifiedName "aws_lb_target_grop" "RestService" "arn"))])

                    (->Resource "RestServiceInstance" "aws_security_group"
                                [(->Attribute "vpc_id"
                                              (->QualifiedName "aws_vpc" "MainVPC" "id"))
                                 (->Attribute "name" "TODO")
                                 (->BlockAttribute "ingress"
                                                   [(->Attribute "from_port" "80")
                                                    (->Attribute "to_port" "80")
                                                    (->Attribute "protocol" "tcp")
                                                    (->Attribute "security_groups" "TODO: array value [aws_security_group.RestServiceLoadBalancer.id]")])
                                 (->BlockAttribute "egress"
                                                   [(->Attribute "from_port" "0")
                                                    (->Attribute "to_port" "0")
                                                    (->Attribute "protocol" "-1")
                                                    (->Attribute "security_groups" "TODO: array value [aws_security_group.RestServiceLoadBalancer.id]")])])
                    (->Resource "RestServiceLoadBalancer" "aws_security_group"
                                [(->Attribute "vpc_id"
                                              (->QualifiedName "aws_vpc" "MainVPC" "id"))
                                 (->Attribute "name" "TODO")
                                 (->BlockAttribute "ingress"
                                                   [(->Attribute "from_port" "80")
                                                    (->Attribute "to_port" "80")
                                                    (->Attribute "protocol" "tcp")
                                                    (->Attribute "cidr_blocks" "TODO: array value [0.0.0.0/0]")])
                                 (->BlockAttribute "egress"
                                                   [(->Attribute "from_port" "0")
                                                    (->Attribute "to_port" "0")
                                                    (->Attribute "protocol" "-1")
                                                    (->Attribute "cidr_blocks" "TODO: array value [0.0.0.0/0]")])])]
                   (translate-full))))))))

