(ns matasano-clj.challenge-one-test
  (:require [clojure.test :refer :all]
            [matasano-clj.challenge-one :refer :all]))

;------ Challenge 1 --------
(deftest hex->b64-test
  (testing "Testing Hex->B64."
    (is (= (hex->b64 hexvals)
           expected))))

(deftest b64->hex-test
  (testing "Testing B64->Hex"
    (is (= (b64->hex expected)
           hexvals))))
;--------- end -----------

;----- Challenge 2 ------
(deftest xor-strs-test
  (testing "Xor 2 Strings"
    (is (= (xor-strs' xor-a xor-b)
           xor-c))))
;-------- end -------
