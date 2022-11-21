(ns cloud-descriptor.translator-test
  (:require [clojure.test :refer :all]
            [cloud-descriptor.core :refer :all]
            [cloud-descriptor.parser :refer :all]
            [cloud-descriptor.symbol-table :refer :all]
            [cloud-descriptor.translator :refer :all]))

(deftest translator-test
  (testing "Translation of"
    (testing "single VPC"
      (testing "empty should throw error"
        (let [input-string "VPC TestVPC {}"]
          (with-sym-tab
            (is (thrown? clojure.lang.ExceptionInfo (input->symbol-table input-string))))))

      (testing "with attributes\n"
        (testing "> without required attributes"
          (let [input-string "VPC TestVPC {cidr_block=10.0.0.0/16}"]
            (with-sym-tab
              (input->symbol-table input-string)
              (is (= ()
                     (translate))))))
        (testing "> with required attributes"))

      (testing "with one subnet"
        (testing "and EC2"))

      (testing "with EC2"))

    (testing "basic infrastructure")))
