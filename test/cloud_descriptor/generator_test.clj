(ns cloud-descriptor.generator-test
  (:require [clojure.test :refer :all]
            [cloud-descriptor.core :refer :all]
            [cloud-descriptor.parser :refer :all]
            [cloud-descriptor.symbol-table :refer :all]
            [cloud-descriptor.translator :refer :all]
            [cloud-descriptor.terraform-domain :refer :all]
            [cloud-descriptor.generator :refer :all]))

(defmacro test-generation
  [input expected-output]
  `(let [input-string# ~input]
     (initialize)
     (with-sym-tab
       (input->symbol-table input-string#)
       (translate-to-tf {}))
     (with-terraform
       (let [output# (generate-resources)]
         ;; todo: better diff
         ;; (println "output:")
         ;; (println output#)
         (is (= ~expected-output
                output#))))))

(deftest generator-test
  (testing "Generation of"
    (testing "VPC"
      (test-generation
       "VPC TestVPC {region = eu-north-1 cidr_block = 10.0.0.0/16}"
       "provider \"aws\" {
  region = \"eu-north-1\"
}

resource \"aws_vpc\" \"TestVPC\" {
  cidr_block = \"10.0.0.0/16\"
  tags = {
    Name = \"TestVPC\"
  }
}")

      (testing "with subnet"
        (test-generation
         "VPC TestVPC {
  region = us-east-2
  cidr_block = 172.16.0.0/16
  Subnet TestSubnet = 172.16.0.0/24 
}"
         "provider \"aws\" {
  region = \"us-east-2\"
}

resource \"aws_vpc\" \"TestVPC\" {
  cidr_block = \"172.16.0.0/16\"
  tags = {
    Name = \"TestVPC\"
  }
}

resource \"aws_subnet\" \"TestSubnet\" {
  vpc_id = aws_vpc.TestVPC.id
  cidr_block = \"172.16.0.0/24\"
  tags = {
    Name = \"TestSubnet\"
  }
}" ;; TODO: availability zone
         )

        (testing "and EC2"
          (test-generation
           "VPC TestVPC {
  cidr_block=10.0.0.0/16
  region=eu-north-1
  Subnet TestSubnet {
    cidr_block = 10.0.6.0/24
    availability_zone = eu-north-1a
    EC2 TestServer {
      ami = ubuntu
      instance_type = t3.micro
      networking {
        ingress = http
        egress = all
      }
    }
  }
}"
           "provider \"aws\" {
  region = \"eu-north-1\"
}

resource \"aws_vpc\" \"TestVPC\" {
  cidr_block = \"10.0.0.0/16\"
  tags = {
    Name = \"TestVPC\"
  }
}

resource \"aws_subnet\" \"TestSubnet\" {
  vpc_id = aws_vpc.TestVPC.id
  cidr_block = \"10.0.6.0/24\"
  availability_zone = \"eu-north-1a\"
  tags = {
    Name = \"TestSubnet\"
  }
}

resource \"aws_instance\" \"TestServer\" {
  subnet_id = aws_subnet.TestSubnet.id
  ami = \"ubuntu\"
  instance_type = \"t3.micro\"
  networking {
    ingress = \"http\"
    egress = \"all\"
  }
  tags = {
    Name = \"TestServer\"
  }
}" ;; TODO: test is incorrect
           ))))))

