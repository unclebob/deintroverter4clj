(ns myapp.pipeline-var-deref-spec
  (:require [speclj.core :refer [describe it should=]]
            [myapp.core :as core]))

(defn- run-private-step [step-var]
  (step-var 42))

(defn- assert-private-failure! [step-var]
  (try
    (run-private-step step-var)
    (throw (ex-info "expected throw" {}))
    (catch clojure.lang.ExceptionInfo ex
      (should= "step failed" (.getMessage ex)))))

(describe "private var deref helper"
  (it "walks through a var deref argument into catch assertions"
    (assert-private-failure! (clojure.core/deref (var core/private-step!)))))