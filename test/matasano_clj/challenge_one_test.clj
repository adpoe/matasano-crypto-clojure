(ns matasano-clj.challenge-one-test
  (:require [clojure.test :refer :all]
            [matasano-clj.challenge-one :refer :all]))

(deftest hex->b64-test
  (testing "Testing Hex->B64."
    (is (= (hex->b64 hexvals)
           expected))))

(deftest b64->hex-test
  (testing "Testing B64->Hex"
    (is (= (b64->hex expected)
           hexvals))))
