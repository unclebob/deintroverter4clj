(ns deintroverter.assertions-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.assertions :as assertions]))

(deftest recognizes-is-with-equals
  (is (= :is (:macro (assertions/parse-assertion '(is (= 42 result))))))
  (is (= '(= 42 result) (:asserted-form (assertions/parse-assertion '(is (= 42 result)))))))

(deftest recognizes-should=
  (is (= :should= (:macro (assertions/parse-assertion '(should= actual expected)))))
  (is (= 'expected (:asserted-form (assertions/parse-assertion '(should= actual expected))))))

(deftest recognizes-speclj-should-macros
  (is (= :should (:macro (assertions/parse-assertion '(should (= 1 x))))))
  (is (= '(= 1 x) (:asserted-form (assertions/parse-assertion '(should (= 1 x))))))
  (is (= 'actual
         (:asserted-form (assertions/parse-assertion '(should-contain coll actual)))))
  (is (= 'actual
         (:asserted-form (assertions/parse-assertion '(should-be-a actual String))))))

(deftest recognizes-should-not-contain
  (is (= :should-not-contain
         (:macro (assertions/parse-assertion '(should-not-contain :key m)))))
  (is (= 'm (:asserted-form (assertions/parse-assertion '(should-not-contain :key m)))))
  (is (= 'items
         (:asserted-form (assertions/parse-assertion '(should-not-contain [1 1] items))))))

(deftest recognizes-should-fail
  (is (= :should-fail (:macro (assertions/parse-assertion '(should-fail "expected exception")))))
  (is (= "expected exception"
         (:asserted-form (assertions/parse-assertion '(should-fail "expected exception"))))))

(deftest recognizes-should-not-be-nil
  (is (= :should-not-be-nil
         (:macro (assertions/parse-assertion '(should-not-be-nil (:line site))))))
  (is (= '(:line site)
         (:asserted-form (assertions/parse-assertion '(should-not-be-nil (:line site)))))))

(deftest recognizes-speclj-throw-macros
  (is (= :should-not-throw
         (:macro (assertions/parse-assertion '(should-not-throw (myapp.core/foo))))))
  (is (= '(myapp.core/foo)
         (:asserted-form (assertions/parse-assertion '(should-not-throw (myapp.core/foo))))))
  (is (= :should-throw
         (:macro (assertions/parse-assertion '(should-throw Exception (myapp.core/foo))))))
  (is (= '(myapp.core/foo)
         (:asserted-form (assertions/parse-assertion '(should-throw Exception (myapp.core/foo))))))
  (is (= '(myapp.core/run-job x)
         (:asserted-form (assertions/parse-assertion
                          '(should-throw clojure.lang.ExceptionInfo (myapp.core/run-job x)))))))

(deftest recognizes-should-greater-than
  (is (= :should> (:macro (assertions/parse-assertion '(should> (count xs) 0)))))
  (is (= '(count xs)
         (:asserted-form (assertions/parse-assertion '(should> (count xs) 0))))))

(deftest recognizes-stub-invocation-macros
  (is (= :should-have-invoked
         (:macro (assertions/parse-assertion
                  '(should-have-invoked :send-message {:with [:a 1]})))))
  (is (= :should-not-have-invoked
         (:macro (assertions/parse-assertion '(should-not-have-invoked :send-message)))))
  (is (assertions/stub-invocation?
       (assertions/parse-assertion '(should-have-invoked :x))))
  (is (nil? (:asserted-form (assertions/parse-assertion '(should-have-invoked :x))))))

(deftest unknown-assertion-like-macro-is-questionable
  (is (= :unknown-assertion-macro
         (:reason (assertions/parse-assertion '(assert-custom x)))))
  (is (= :unknown-assertion-macro
         (:reason (assertions/parse-assertion '(should-unknown x))))))

(deftest non-assertion-forms-return-nil
  (is (nil? (assertions/parse-assertion '(with-temp-source-path (fn [])))))
  (is (nil? (assertions/parse-assertion '(myapp.core/foo x))))
  (is (nil? (assertions/parse-assertion '((var myapp.core/foo) x)))))