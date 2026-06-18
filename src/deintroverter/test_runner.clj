(ns deintroverter.test-runner
  (:require [clojure.test :refer [run-tests]]
            deintroverter.smoke-test))

(defn run [_]
  (let [{:keys [fail error]} (run-tests 'deintroverter.smoke-test)]
    (when (pos? (+ fail error))
      (System/exit 1))))