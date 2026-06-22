(ns myapp.exception-catch-assertion-spec
  (:require [speclj.core :refer [describe it should=]]
            [myapp.core :as core]))

(defn- assert-parse-failure! []
  (try
    (core/parse-dimensions ["200" "100"] 1000 1000)
    (throw (ex-info "expected throw" {}))
    (catch clojure.lang.ExceptionInfo ex
      (should= 90 (:max-cols (ex-data ex)))
      (should= "too large" (.getMessage ex)))))

(describe "exception catch assertions"
  (it "promotes assertions on a catch-bound exception after sut call"
    (assert-parse-failure!))

  (it "promotes assertions on ex-data bound from a catch exception"
    (try
      (core/parse-dimensions ["200" "100"] 1000 1000)
      (should-fail "expected exception")
      (catch clojure.lang.ExceptionInfo e
        (let [data (ex-data e)]
          (should= 90 (:max-cols data))))))

  (it "stays introverted when catch assertions lack a preceding sut call"
    (try
      (throw (ex-info "boom" {:max-cols 1}))
      (catch clojure.lang.ExceptionInfo ex
        (should= 1 (:max-cols (ex-data ex)))))))