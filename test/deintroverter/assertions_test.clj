(ns deintroverter.assertions-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.assertions :as assertions]))

(deftest recognizes-is-with-equals
  (is (= :is (:macro (assertions/parse-assertion '(is (= 42 result))))))
  (is (= '(= 42 result) (:asserted-form (assertions/parse-assertion '(is (= 42 result)))))))

(deftest recognizes-should=
  (is (= :should= (:macro (assertions/parse-assertion '(should= actual expected)))))
  (is (= 'expected (:asserted-form (assertions/parse-assertion '(should= actual expected))))))

(deftest unknown-macro-is-questionable
  (is (= :unknown-assertion-macro
         (:reason (assertions/parse-assertion '(assert-custom x))))))