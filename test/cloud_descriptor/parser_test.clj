(ns cloud-descriptor.parser-test
  (:require [clojure.test :refer :all]
            [cloud-descriptor.core :refer :all]
            [cloud-descriptor.parser :refer :all]))

;;todo: a better way to test for exceptions
;; todo: split to nested tests
(deftest parser-test
  (testing "Parsing"
    (testing "empty string"
      (is (thrown? clojure.lang.ExceptionInfo
                   (parse "")))
      (is (thrown? clojure.lang.ExceptionInfo
                   (parse "      "))
          "> with whitespaces"))
    (testing "single resource"
      (testing "without init"
        (let [input-string "EC2 TestEC2"]
          (is (= [:S [:Resource
                      [:ResourceType "EC2"]
                      [:ResourceName "TestEC2"]]]
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
          (let [input-string "EC2  "]
            (is (thrown? clojure.lang.ExceptionInfo
                         (parse input-string))
                "> EC2 with name not starting from a letter")))))))
