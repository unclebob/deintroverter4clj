(ns deintroverter.test-runner
  (:require [clojure.test :refer [run-tests]]
            deintroverter.smoke-test
            deintroverter.paths-test
            deintroverter.parse-test
            deintroverter.project-test
            deintroverter.sut-test
            deintroverter.trace-test
            deintroverter.assertions-test
            deintroverter.analyze-test
            deintroverter.report-test
            deintroverter.core-test
            deintroverter.speclj-test))

(def ^:private test-namespaces
  '[deintroverter.smoke-test
    deintroverter.paths-test
    deintroverter.parse-test
    deintroverter.project-test
    deintroverter.sut-test
    deintroverter.trace-test
    deintroverter.assertions-test
    deintroverter.analyze-test
    deintroverter.report-test
    deintroverter.core-test
    deintroverter.speclj-test])

(defn run [_]
  (let [result (apply run-tests test-namespaces)
        {:keys [test fail error]} result]
    (println (str "\nRan " test " tests, " fail " failures, " error " errors."))
    (when (pos? (+ fail error))
      (System/exit 1))))