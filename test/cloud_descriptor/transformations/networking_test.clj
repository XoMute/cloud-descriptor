(ns cloud-descriptor.transformations.networking-test
  (:require [clojure.test :refer :all]
            [cloud-descriptor.transformations.networking :refer :all]
            [cloud-descriptor.core :refer :all]
            [cloud-descriptor.symbol-table :refer :all]
            [cloud-descriptor.translator :refer :all]
            [cloud-descriptor.terraform-domain :refer :all]
            [cloud-descriptor.generator :refer :all]))

(defmacro test-transformation
  [input expected-output]
  `(do
     (initialize)
     (with-sym-tab
       (input->symbol-table ~input)
       (translate-to-tf {:networking true
                         :auto-generate-cidr-blocks true}))
     (with-terraform
       (let [output# (generate-resources)]
         ;; todo: better diff
         ;; (println "output:")
         ;; (println output#)
         (is (= ~expected-output
                output#))))))

(deftest test-networking-wrong-inputs
  (testing "Networking transformation:"
    (testing "networking with no init"
      (let [input "VPC TestVPC {
  region = eu-north-1
  Subnet TestSubnet {
    EC2 TestEC2 {
      ami = ubuntu
      instance_type = t3.micro

      networking
    }
  }
}"]
        (initialize)
        (with-sym-tab
          (is (thrown? clojure.lang.ExceptionInfo
                       (input->symbol-table input))))))

    (testing "networking with no rules"
      (let [input "VPC TestVPC {
  region = eu-north-1
  Subnet TestSubnet {
    EC2 TestEC2 {
      ami = ubuntu
      instance_type = t3.micro

      networking {}
    }
  }
}"]
        (initialize)
        (with-sym-tab
          (is (thrown? clojure.lang.ExceptionInfo
                       (input->symbol-table input))))))))

(deftest test-networking
  (testing "Networking transformation:"
    (testing "networking with egress rule should generate nat gateway"
      (test-transformation
       "VPC TestVPC {
  region = eu-north-1
  Subnet TestSubnet {
    EC2 TestEC2 {
      ami = ubuntu
      instance_type = t3.micro

      networking {
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
  cidr_block = \"10.0.1.0/24\"
  tags = {
    Name = \"TestSubnet\"
  }
}

resource \"aws_instance\" \"TestEC2\" {
  ami = \"ubuntu\"
  instance_type = \"t3.micro\"
  network_interface {
    device_index = \"0\"
    network_interface_id = aws_network_interface.TestEC2.id
  }
  tags = {
    Name = \"TestEC2\"
  }
}

resource \"aws_security_group\" \"TestEC2\" {
  vpc_id = aws_vpc.TestVPC.id
  egress {
    protocol = \"-1\"
    from_port = \"0\"
    to_port = \"0\"
    cidr_blocks = [\"0.0.0.0/0\"]
  }
  tags = {
    Name = \"TestEC2-sg\"
  }
}

resource \"aws_network_interface\" \"TestEC2\" {
  subnet_id = aws_subnet.TestSubnet.id
  private_ips = [\"10.0.1.4\"]
  security_groups = [aws_security_group.TestEC2.id]
  tags = {
    Name = \"TestEC2-ni\"
  }
}

resource \"aws_eip\" \"TestEC2\" {
  vpc = \"true\"
  tags = {
    Name = \"TestEC2-eip\"
  }
}

resource \"aws_nat_gateway\" \"TestEC2\" {
  allocation_id = aws_eip.TestEC2.id
  subnet_id = aws_subnet.TestSubnet.id
  tags = {
    Name = \"TestEC2-nat\"
  }
}

resource \"aws_route_table\" \"TestEC2\" {
  vpc_id = aws_vpc.TestVPC.id
  route {
    cidr_block = \"0.0.0.0/0\"
    nat_gateway_id = aws_nat_gateway.TestEC2.id
  }
  tags = {
    Name = \"TestEC2-rt\"
  }
}

resource \"aws_route_table_association\" \"TestEC2\" {
  subnet_id = aws_subnet.TestSubnet.id
  route_table_id = aws_route_table.TestEC2.id
}"))

    (testing "networking with ingress rule should generate public ip for the instance"
      (test-transformation
       "VPC TestVPC {
  region = eu-north-1
  Subnet TestSubnet {
    EC2 TestEC2 {
      ami = ubuntu
      instance_type = t3.micro

      networking {
        ingress = ssh
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
  cidr_block = \"10.0.1.0/24\"
  tags = {
    Name = \"TestSubnet\"
  }
}

resource \"aws_instance\" \"TestEC2\" {
  ami = \"ubuntu\"
  instance_type = \"t3.micro\"
  network_interface {
    device_index = \"0\"
    network_interface_id = aws_network_interface.TestEC2.id
  }
  tags = {
    Name = \"TestEC2\"
  }
}

resource \"aws_security_group\" \"TestEC2\" {
  vpc_id = aws_vpc.TestVPC.id
  ingress {
    protocol = \"tcp\"
    from_port = \"22\"
    to_port = \"22\"
    cidr_blocks = [\"0.0.0.0/0\"]
  }
  tags = {
    Name = \"TestEC2-sg\"
  }
}

resource \"aws_network_interface\" \"TestEC2\" {
  subnet_id = aws_subnet.TestSubnet.id
  private_ips = [\"10.0.1.4\"]
  security_groups = [aws_security_group.TestEC2.id]
  tags = {
    Name = \"TestEC2-ni\"
  }
}

resource \"aws_internet_gateway\" \"TestEC2\" {
  vpc_id = aws_vpc.TestVPC.id
  tags = {
    Name = \"TestEC2-igw\"
  }
}

resource \"aws_eip\" \"TestEC2\" {
  vpc = \"true\"
  network_interface = aws_network_interface.TestEC2.id
  associate_with_private_ip = \"10.0.1.4\"
  tags = {
    Name = \"TestEC2-eip\"
  }
}"))

    (testing "networking with egress and ingress rule should generate public ip for the instance"
      (test-transformation
       "VPC TestVPC {
  region = eu-north-1
  Subnet TestSubnet {
    EC2 TestEC2 {
      ami = ubuntu
      instance_type = t3.micro

      networking {
        egress = all
        ingress = http
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
  cidr_block = \"10.0.1.0/24\"
  tags = {
    Name = \"TestSubnet\"
  }
}

resource \"aws_instance\" \"TestEC2\" {
  ami = \"ubuntu\"
  instance_type = \"t3.micro\"
  network_interface {
    device_index = \"0\"
    network_interface_id = aws_network_interface.TestEC2.id
  }
  tags = {
    Name = \"TestEC2\"
  }
}

resource \"aws_security_group\" \"TestEC2\" {
  vpc_id = aws_vpc.TestVPC.id
  egress {
    protocol = \"-1\"
    from_port = \"0\"
    to_port = \"0\"
    cidr_blocks = [\"0.0.0.0/0\"]
  }
  ingress {
    protocol = \"tcp\"
    from_port = \"80\"
    to_port = \"80\"
    cidr_blocks = [\"0.0.0.0/0\"]
  }
  tags = {
    Name = \"TestEC2-sg\"
  }
}

resource \"aws_network_interface\" \"TestEC2\" {
  subnet_id = aws_subnet.TestSubnet.id
  private_ips = [\"10.0.1.4\"]
  security_groups = [aws_security_group.TestEC2.id]
  tags = {
    Name = \"TestEC2-ni\"
  }
}

resource \"aws_internet_gateway\" \"TestEC2\" {
  vpc_id = aws_vpc.TestVPC.id
  tags = {
    Name = \"TestEC2-igw\"
  }
}

resource \"aws_eip\" \"TestEC2\" {
  vpc = \"true\"
  network_interface = aws_network_interface.TestEC2.id
  associate_with_private_ip = \"10.0.1.4\"
  tags = {
    Name = \"TestEC2-eip\"
  }
}"))

    (testing "networking with all ingress and egress rules available"
      (test-transformation
       "VPC TestVPC {
  region = eu-north-1
  Subnet TestSubnet {
    EC2 TestEC2 {
      ami = ubuntu
      instance_type = t3.micro

      networking {
        ingress = [all, ssh, http, https]
        egress  = [all, ssh, http, https]
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
  cidr_block = \"10.0.1.0/24\"
  tags = {
    Name = \"TestSubnet\"
  }
}

resource \"aws_instance\" \"TestEC2\" {
  ami = \"ubuntu\"
  instance_type = \"t3.micro\"
  network_interface {
    device_index = \"0\"
    network_interface_id = aws_network_interface.TestEC2.id
  }
  tags = {
    Name = \"TestEC2\"
  }
}

resource \"aws_security_group\" \"TestEC2\" {
  vpc_id = aws_vpc.TestVPC.id
  ingress {
    protocol = \"-1\"
    from_port = \"0\"
    to_port = \"0\"
    cidr_blocks = [\"0.0.0.0/0\"]
  }
  ingress {
    protocol = \"tcp\"
    from_port = \"22\"
    to_port = \"22\"
    cidr_blocks = [\"0.0.0.0/0\"]
  }
  ingress {
    protocol = \"tcp\"
    from_port = \"80\"
    to_port = \"80\"
    cidr_blocks = [\"0.0.0.0/0\"]
  }
  ingress {
    protocol = \"tcp\"
    from_port = \"443\"
    to_port = \"443\"
    cidr_blocks = [\"0.0.0.0/0\"]
  }
  egress {
    protocol = \"-1\"
    from_port = \"0\"
    to_port = \"0\"
    cidr_blocks = [\"0.0.0.0/0\"]
  }
  egress {
    protocol = \"tcp\"
    from_port = \"22\"
    to_port = \"22\"
    cidr_blocks = [\"0.0.0.0/0\"]
  }
  egress {
    protocol = \"tcp\"
    from_port = \"80\"
    to_port = \"80\"
    cidr_blocks = [\"0.0.0.0/0\"]
  }
  egress {
    protocol = \"tcp\"
    from_port = \"443\"
    to_port = \"443\"
    cidr_blocks = [\"0.0.0.0/0\"]
  }
  tags = {
    Name = \"TestEC2-sg\"
  }
}

resource \"aws_network_interface\" \"TestEC2\" {
  subnet_id = aws_subnet.TestSubnet.id
  private_ips = [\"10.0.1.4\"]
  security_groups = [aws_security_group.TestEC2.id]
  tags = {
    Name = \"TestEC2-ni\"
  }
}

resource \"aws_internet_gateway\" \"TestEC2\" {
  vpc_id = aws_vpc.TestVPC.id
  tags = {
    Name = \"TestEC2-igw\"
  }
}

resource \"aws_eip\" \"TestEC2\" {
  vpc = \"true\"
  network_interface = aws_network_interface.TestEC2.id
  associate_with_private_ip = \"10.0.1.4\"
  tags = {
    Name = \"TestEC2-eip\"
  }
}"))))

