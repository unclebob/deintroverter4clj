(ns deintroverter.report-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [deintroverter.report :as report]))

(def sample-findings
  [{:file "t.clj" :line 10 :test-name "a" :test-form :deftest
    :verdict :introverted :reason :no-sut-assertion :sut-namespaces #{'myapp.core}}
   {:file "t.clj" :line 20 :test-name "b" :test-form :it
    :verdict :extroverted :reason nil :sut-namespaces #{'myapp.core}}])

(deftest human-output-hides-extroverted-by-default
  (is (str/includes?
       (with-out-str (report/print-human sample-findings false))
       "introverted"))
  (is (not (str/includes?
            (with-out-str (report/print-human sample-findings false))
            "(it b)  :extroverted"))))

(deftest human-output-hides-likely-extroverted-by-default
  (let [findings (conj sample-findings
                       {:file "t.clj" :line 30 :test-name "c" :test-form :it
                        :verdict :likely-extroverted :reason :refer-all-heuristic
                        :sut-namespaces #{'myapp.core}})]
    (is (not (str/includes?
              (with-out-str (report/print-human findings false))
              "(it c)  :likely-extroverted")))))

(deftest exit-code-1-when-introverted-or-questionable
  (is (= 1 (report/exit-code sample-findings [])))
  (is (= 0 (report/exit-code [{:verdict :extroverted}] [])))
  (is (= 0 (report/exit-code [{:verdict :likely-extroverted}] []))))