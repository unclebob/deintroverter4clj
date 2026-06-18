(ns deintroverter.assertions-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.assertions :as assertions]))

(deftest recognizes-is-with-equals
  (is (= {:macro :is :asserted-form '(= 42 result) :reason nil}
         (assertions/parse-assertion '(is (= 42 result))))))

(deftest recognizes-should=
  (is (= {:macro :should= :asserted-form 'expected :reason nil}
         (assertions/parse-assertion '(should= actual expected)))))

(deftest unknown-macro-is-questionable
  (is (= {:macro nil :asserted-form nil :reason :unknown-assertion-macro}
         (assertions/parse-assertion '(assert-custom x)))))