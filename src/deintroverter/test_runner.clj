(ns deintroverter.test-runner
  (:require [clojure.test :refer [run-tests]]
            [deintroverter.runner-support :as runner-support]))

(def ^:private test-namespaces
  '[deintroverter.smoke-test
    deintroverter.paths-test
    deintroverter.parse-test
    deintroverter.project-test
    deintroverter.sut-test
    deintroverter.test-modules-test
    deintroverter.trace-test
    deintroverter.stack-depth-test
    deintroverter.assertions-test
    deintroverter.analyze-test
    deintroverter.report-test
    deintroverter.core-test
    deintroverter.speclj-test])

(defn run [_]
  (doseq [ns-sym test-namespaces]
    (require ns-sym))
  (let [result (apply run-tests test-namespaces)
        {:keys [test fail error]} result]
    (println (str "\nRan " test " tests, " fail " failures, " error " errors."))
    (runner-support/exit-if-failed! result)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-06-19T12:53:17.557511-05:00", :module-hash "2098873315", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 16, :hash "1470038655"} {:id "def/test-namespaces", :kind "def", :line 18, :end-line 31, :hash "1458480864"} {:id "defn/run", :kind "defn", :line 33, :end-line 37, :hash "725974491"}]}
;; clj-mutate-manifest-end
