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

(deftest exit-code-is-always-zero
  (is (= 0 (report/exit-code sample-findings [])))
  (is (= 0 (report/exit-code [{:verdict :introverted}] [])))
  (is (= 0 (report/exit-code [{:verdict :cloistered}] [])))
  (is (= 0 (report/exit-code [{:verdict :questionable}] [])))
  (is (= 0 (report/exit-code [] [{:type :parse-error}]))))

(deftest summary-groups-conditional-assertions-by-reason
  (let [findings [{:verdict :conditional-assertion :reason :would-be-extroverted}
                  {:verdict :conditional-assertion :reason :would-be-extroverted}
                  {:verdict :conditional-assertion :reason :no-sut-assertion}]
        summary (:summary (report/build-edn "/proj" findings []))]
    (is (= {:total 3 :would-be-extroverted 2 :no-sut-assertion 1}
           (:conditional-assertion summary)))))

(deftest human-output-shows-conditional-assertions
  (let [findings [{:file "t.clj" :line 10 :test-name "maybe" :test-form :it
                   :verdict :conditional-assertion :reason :would-be-extroverted}]]
    (is (str/includes?
         (with-out-str (report/print-human findings false))
         ":conditional-assertion"))))

(deftest human-output-shows-conditional-cause
  (let [findings [{:file "t.clj" :line 10 :test-name "table" :test-form :deftest
                   :verdict :conditional-assertion :reason :would-be-extroverted
                   :conditional-cause :missing-doseq-guard
                   :conditional-context {:head 'doseq :coll 'rows}}]]
    (let [out (with-out-str (report/print-human findings false))]
      (is (str/includes? out "cause: missing doseq guard on rows")))))

(deftest summary-groups-conditional-causes
  (let [findings [{:verdict :conditional-assertion :reason :would-be-extroverted
                   :conditional-cause :missing-doseq-guard}
                  {:verdict :conditional-assertion :reason :would-be-extroverted
                   :conditional-cause :missing-doseq-guard}
                  {:verdict :conditional-assertion :reason :would-be-extroverted
                   :conditional-cause :runtime-conditional}]
        summary (:summary (report/build-edn "/proj" findings []))]
    (is (= {:missing-doseq-guard 2 :runtime-conditional 1}
           (get-in summary [:conditional-assertion :by-cause])))))

(deftest summary-groups-questionable-and-introverted-by-reason
  (let [findings (concat sample-findings
                         [{:verdict :questionable :reason :unknown-assertion-macro}
                          {:verdict :questionable :reason :unknown-assertion-macro}
                          {:verdict :questionable :reason :destructuring}])
        summary (:summary (report/build-edn "/proj" findings []))]
    (is (= {:total 1 :no-sut-assertion 1} (:introverted summary)))
    (is (= {:total 3 :unknown-assertion-macro 2 :destructuring 1}
           (:questionable summary)))))