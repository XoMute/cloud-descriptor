(ns cloud-descriptor.translator-test
  (:require [clojure.test :refer :all]
            [cloud-descriptor.core :refer :all]
            [cloud-descriptor.parser :refer :all]
            [cloud-descriptor.symbol-table :refer :all]
            [cloud-descriptor.translator :refer :all]
            [cloud-descriptor.terraform-domain :refer :all]))

;; todo: test for auto-generation of attributes and resources

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
        (testing "> with auto-generated cidr_block"))

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
                                [(->Attribute "cidr_block" "10.0.5.0/24")])]
                   (translate-sym-tab)))))

        (testing "with auto-generated cidr_block")
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
              (input->symbol-table input-string)
              (is (thrown? clojure.lang.ExceptionInfo (translate-sym-tab))))))

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
                                    [(->Attribute "cidr_block" "10.0.6.0/24")
                                     (->Attribute "availability_zone" "eu-north-1a")])
                        (->Resource "TestServer" "aws_instance"
                                    [(->Attribute "ami" "ubuntu")
                                     (->Attribute "instance_type" "t3.micro")])]
                       (translate-sym-tab))))))))

      (testing "with EC2"))

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
                               (->Attribute "region" "eu-north-1")])
                  (->Resource "ExampleSubnet" "aws_subnet"
                              [] ;; TODO: auto-generate 'cidr_block'
                              )
                  (->Resource "ExampleEC2" "aws_instance"
                              [(->Attribute "ami" "ami-078e13ebe3b027f1c")
                               (->Attribute "instance_type" "t3.micro")
                               (->BlockAttribute "networking"
                                                 [(->Attribute "ingress" "all")])])]
                 (translate-sym-tab))))))))

