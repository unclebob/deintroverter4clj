(ns deintroverter.smoke-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.report :as report]
            [deintroverter.runner-support :as runner-support]))

(deftest clean-run-exits-zero
  (is (= 0 (report/exit-code [] []))))

(deftest runner-exit-if-failed-ignores-clean-run
  (is (nil? (runner-support/exit-if-failed! {:fail 0 :error 0}))))

(deftest runner-exit-if-failed-exits-on-failures
  (is (thrown? Exception
        (binding [runner-support/*exit-fn* (fn [_] (throw (ex-info "exit" {})))]
          (runner-support/exit-if-failed! {:fail 1 :error 0})))))