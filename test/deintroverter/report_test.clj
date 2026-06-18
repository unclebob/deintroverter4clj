(ns deintroverter.report-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.report :as report]))

(def sample-findings
  [{:file "t.clj" :line 10 :test-name "a" :test-form :deftest
    :verdict :introverted :reason :no-sut-assertion :sut-namespaces #{'myapp.core}}
   {:file "t.clj" :line 20 :test-name "b" :test-form :it
    :verdict :extroverted :reason nil :sut-namespaces #{'myapp.core}}])

(deftest human-output-hides-extroverted-by-default
  (let [out (with-out-str (report/print-human sample-findings false))]
    (is (re-find #"introverted" out))
    (is (not (re-find #"\(:extroverted\)" out)))
    (is (not (re-find #"  \(it b\)  :extroverted" out)))))

(deftest exit-code-1-when-introverted-or-questionable
  (is (= 1 (report/exit-code sample-findings [])))
  (is (= 0 (report/exit-code [{:verdict :extroverted}] []))))