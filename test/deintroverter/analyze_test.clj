(ns deintroverter.analyze-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [deintroverter.analyze :as analyze]
            [deintroverter.project :as project]
            [deintroverter.sut :as sut]))

(defn- fixture [name]
  (.getPath (io/file "test/deintroverter/fixtures" name)))

(def project-ctx
  (project/load-context "test/deintroverter/fixtures/sample-project"))

(defn- sut-for [test-ns requires]
  (sut/infer-sut-namespaces
   {:test-namespace test-ns :requires requires
    :project-ctx project-ctx :add #{} :remove #{}}))

(defn- analyze [file test-ns requires]
  (analyze/analyze-file (fixture file)
                        {:sut (sut-for test-ns requires)
                         :project-ctx project-ctx}))

(deftest classifies-extroverted-deftest
  (is (= 1 (count (analyze "extroverted_direct.clj" 'myapp.core-test #{'myapp.core}))))
  (is (= :extroverted (:verdict (first (analyze "extroverted_direct.clj"
                                                'myapp.core-test #{'myapp.core}))))))

(deftest classifies-introverted-deftest
  (is (= :introverted (:verdict (first (analyze "introverted_literal.clj"
                                                'myapp.core-test #{}))))))

(deftest analyzes-file-with-docstring-defn
  (let [findings (analyze "defn_docstring_helper.clj"
                          'myapp.defn-doc-test
                          #{'myapp.core})]
    (is (= 1 (count findings)))
    (is (= :deftest (:test-form (first findings))))))

(deftest classifies-cloistered-when-reaching-test-module
  (is (= :cloistered (:verdict (first (analyze "cloistered_helpers.clj"
                                                'myapp.cloistered-spec
                                                #{'myapp.core})))))
  (is (= :reaches-test-module (:reason (first (analyze "cloistered_helpers.clj"
                                                        'myapp.cloistered-spec
                                                        #{'myapp.core}))))))

(deftest classifies-cloistered-when-reaching-test-module-via-refer-all
  (let [findings (analyze "cloistered_refer_all.clj"
                          'myapp.cloistered-refer-all-spec
                          #{'myapp.core})]
    (is (= 2 (count findings)))
    (is (every? #(= :cloistered (:verdict %)) findings))
    (is (every? #(= :reaches-test-module (:reason %)) findings))))

(deftest classifies-stamping-negative-assertions-not-questionable
  (let [findings (analyze "empire_stamping_negative.clj" 'myapp.stamping-negative-spec #{})]
    (is (= 1 (count findings)))
    (is (= :introverted (:verdict (first findings))))))

(deftest classifies-pipeline-assert-failure-helper
  (let [findings (analyze "pipeline_assert_failure.clj" 'myapp.pipeline-assert-failure-spec #{})]
    (is (= 1 (count findings)))
    (is (not= :questionable (:verdict (first findings))))))

(deftest classifies-sut-side-effect-as-likely-extroverted
  (let [findings (analyze "side_effect_helpers.clj" 'myapp.side-effect-spec #{'myapp.core})]
    (is (= 3 (count findings)))
    (is (= :likely-extroverted (:verdict (first findings))))
    (is (= :sut-side-effect-heuristic (:reason (first findings))))
    (is (= :immediate-preceding-sut
           (get-in (first findings) [:trace :assertions 0 :side-effect-evidence])))
    (is (= :likely-extroverted (:verdict (second findings))))
    (is (= :test-state-binding
           (get-in (second findings) [:trace :assertions 0 :side-effect-evidence])))
    (is (= :cloistered (:verdict (nth findings 2))))))

(deftest classifies-stub-capture-wiring-as-likely-extroverted
  (let [findings (analyze "stub_capture_wiring.clj" 'myapp.stub-capture-wiring-spec #{'myapp.core})]
    (is (= 2 (count findings)))
    (is (= :likely-extroverted (:verdict (first findings))))
    (is (= :sut-wiring-heuristic (:reason (first findings))))
    (is (= :stub-capture
           (get-in (first findings) [:trace :assertions 0 :wiring-evidence])))
    (is (= :introverted (:verdict (second findings))))
    (is (= :no-sut-assertion (:reason (second findings))))))

(deftest classifies-callback-spy-wiring-as-likely-extroverted
  (let [findings (analyze "callback_spy_wiring.clj" 'myapp.callback-spy-wiring-spec #{'myapp.core})]
    (is (= 1 (count findings)))
    (is (= :likely-extroverted (:verdict (first findings))))
    (is (= :sut-wiring-heuristic (:reason (first findings))))
    (is (= :stub-capture (get-in (first findings) [:trace :assertions 0 :wiring-evidence])))))

(deftest classifies-nested-spy-wiring-as-likely-extroverted
  (let [findings (analyze "nested_spy_wiring.clj" 'myapp.nested-spy-wiring-spec #{'myapp.core})]
    (is (= 1 (count findings)))
    (is (= :likely-extroverted (:verdict (first findings))))
    (is (= :sut-wiring-heuristic (:reason (first findings))))))

(deftest classifies-sut-defined-atom-reads-as-extroverted-not-wiring
  (let [findings (analyze "sut_atom_side_effect.clj" 'myapp.sut-atom-side-effect-spec #{'myapp.core})]
    (is (= 3 (count findings)))
    (is (every? #(= :extroverted (:verdict %)) findings))
    (is (every? #(nil? (get-in % [:trace :assertions 0 :wiring-evidence])) findings))))

(deftest classifies-world-atom-readback-as-likely-extroverted
  (let [findings (analyze "world_atom_readback.clj" 'myapp.world-atom-readback-spec #{'myapp.core})]
    (is (= 2 (count findings)))
    (is (= :likely-extroverted (:verdict (first findings))))
    (is (= :sut-side-effect-heuristic (:reason (first findings))))
    (is (= :world-atom-readback
           (get-in (first findings) [:trace :assertions 0 :side-effect-evidence])))
    (is (= :world-atom-readback
           (get-in (first findings) [:trace :assertions 1 :side-effect-evidence])))
    (is (= :introverted (:verdict (second findings))))))

(deftest classifies-sut-result-read-as-likely-extroverted
  (let [findings (analyze "debug_sut_result.clj" 'myapp.debug-sut-result-spec #{'myapp.core})]
    (is (= 2 (count findings)))
    (is (= :likely-extroverted (:verdict (first findings))))
    (is (= :sut-side-effect-heuristic (:reason (first findings))))
    (is (= :sut-result-read
           (get-in (first findings) [:trace :assertions 0 :side-effect-evidence])))
    (is (= :sut-result-read
           (get-in (first findings) [:trace :assertions 1 :side-effect-evidence])))
    (is (= :introverted (:verdict (second findings))))))

(deftest classifies-sut-invoke-in-assertion-as-extroverted
  (let [findings (analyze "assertion_sut_invoke.clj" 'myapp.assertion-sut-invoke-spec #{'myapp.core})]
    (is (= 3 (count findings)))
    (is (= :extroverted (:verdict (first findings))))
    (is (= :likely-extroverted (:verdict (second findings))))
    (is (= :sut-direct-assertion-heuristic (:reason (second findings))))
    (is (= :sut-invoke
           (get-in (second findings) [:trace :assertions 0 :direct-assertion-evidence])))
    (is (= :introverted (:verdict (nth findings 2))))))

(deftest classifies-nested-sut-in-assertion-as-extroverted
  (let [findings (analyze "nested_sut_assertion.clj" 'myapp.nested-sut-assertion-spec #{'myapp.core 'myapp.debug})]
    (is (= 2 (count findings)))
    (is (= :extroverted (:verdict (first findings))))
    (is (= :extroverted (get-in (first findings) [:trace :assertions 0 :verdict])))
    (is (= :introverted (:verdict (second findings))))))

(deftest classifies-exception-catch-assertion-as-likely-extroverted
  (let [findings (analyze "exception_catch_assertion.clj" 'myapp.exception-catch-assertion-spec #{'myapp.core})]
    (is (= 3 (count findings)))
    (is (= :likely-extroverted (:verdict (first findings))))
    (is (= :sut-side-effect-heuristic (:reason (first findings))))
    (is (= :exception-catch-assertion
           (get-in (first findings) [:trace :assertions 0 :side-effect-evidence])))
    (is (= :exception-catch-assertion
           (get-in (first findings) [:trace :assertions 1 :side-effect-evidence])))
    (is (= :likely-extroverted (:verdict (second findings))))
    (is (= :exception-catch-assertion
           (get-in (second findings) [:trace :assertions 1 :side-effect-evidence])))
    (is (= :introverted (:verdict (nth findings 2))))))

(deftest classifies-pipeline-var-deref-helper-as-likely-extroverted
  (let [findings (analyze "pipeline_var_deref.clj" 'myapp.pipeline-var-deref-spec #{'myapp.core})]
    (is (= 1 (count findings)))
    (is (= :likely-extroverted (:verdict (first findings))))
    (is (= :exception-catch-assertion
           (get-in (first findings) [:trace :assertions 0 :side-effect-evidence])))))

(deftest classifies-speclj-with-fixture-as-extroverted
  (let [findings (analyze "speclj_with_fixture.clj" 'myapp.speclj-with-fixture-spec #{'myapp.core})]
    (is (= 2 (count findings)))
    (is (= :extroverted (:verdict (first findings))))
    (is (= :introverted (:verdict (second findings))))))

(deftest classifies-helper-destructure-result-as-likely-extroverted
  (let [findings (analyze "helper_destructure_result.clj" 'myapp.helper-destructure-result-spec #{'myapp.core})]
    (is (= 2 (count findings)))
    (is (= :likely-extroverted (:verdict (first findings))))
    (is (= :sut-direct-assertion-heuristic (:reason (first findings))))
    (is (= :nested-sut-invoke
           (get-in (first findings) [:trace :assertions 0 :direct-assertion-evidence])))
    (is (= :introverted (:verdict (second findings))))))

(deftest classifies-with-redefs-world-readback-as-likely-extroverted
  (let [findings (analyze "with_redefs_world_readback.clj"
                          'myapp.with-redefs-world-readback-spec
                          #{'myapp.core})]
    (is (= 1 (count findings)))
    (is (= :likely-extroverted (:verdict (first findings))))
    (is (= :world-atom-readback
           (get-in (first findings) [:trace :assertions 0 :side-effect-evidence])))))

(deftest classifies-file-dependency-as-likely-extroverted
  (let [findings (analyze "file_dependency.clj" 'myapp.file-dependency-spec #{'myapp.core})]
    (is (= 2 (count findings)))
    (is (= :likely-extroverted (:verdict (first findings))))
    (is (= :file-dependency (:reason (first findings))))
    (is (= :file-dependency
           (get-in (first findings) [:trace :assertions 0 :external-dependency-evidence])))
    (is (= :introverted (:verdict (second findings))))
    (is (= :no-sut-assertion (:reason (second findings))))))

(deftest walks-assertions-inside-binding
  (let [findings (analyze "binding_output.clj" 'myapp.binding-output-spec #{'myapp.core})]
    (is (= 2 (count findings)))
    (is (pos? (count (:assertions (:trace (first findings))))))
    (is (pos? (count (:assertions (:trace (second findings))))))
    (is (not= :no-assertions (:reason (first findings))))
    (is (not= :no-assertions (:reason (second findings))))))

(deftest classifies-cloistered-via-alias-extra-paths
  (is (= :cloistered (:verdict (first (analyze "cloistered_spec_mother.clj"
                                                'myapp.spec-mother-spec
                                                #{'myapp.core}))))))

(deftest classifies-extroverted-vector-destructure
  (is (= :extroverted (:verdict (first (analyze "questionable_destructure.clj"
                                                 'myapp.core-test #{'myapp.core}))))))

(deftest classifies-extroverted-map-keys-destructure
  (is (= :extroverted (:verdict (first (analyze "extroverted_destructure_keys.clj"
                                                 'myapp.destructure-keys-test
                                                 #{'myapp.core}))))))

(deftest classifies-extroverted-rest-destructure
  (is (= :extroverted (:verdict (first (analyze "extroverted_destructure_rest.clj"
                                                 'myapp.destructure-rest-test
                                                 #{'myapp.core}))))))

(deftest classifies-extroverted-nested-destructure
  (is (= :extroverted (:verdict (first (analyze "questionable_nested_destructure.clj"
                                                 'myapp.nested-destructure-test
                                                 #{'myapp.core}))))))

(deftest classifies-extroverted-symbol-key-map-destructure
  (is (= :extroverted (:verdict (first (analyze "extroverted_destructure_symbol_keys.clj"
                                                 'myapp.destructure-symbol-keys-test
                                                 #{'myapp.core}))))))

(deftest classifies-extroverted-nested-vector-pairs-destructure
  (is (= :extroverted (:verdict (first (analyze "extroverted_destructure_nested_vectors.clj"
                                                 'myapp.destructure-nested-vectors-test
                                                 #{'myapp.core}))))))

(deftest classifies-speclj-wrappers-as-extroverted
  (let [findings (analyze "speclj_wrappers.clj" 'myapp.wrapper-spec #{'myapp.core})]
    (is (= 3 (count findings)))
    (is (every? #(= :extroverted (:verdict %)) findings))))

(deftest classifies-through-setup-forms
  (let [findings (analyze "speclj_setup.clj" 'myapp.setup-spec #{'myapp.core})]
    (is (= 2 (count findings)))
    (is (every? #(= :extroverted (:verdict %)) findings))))

(deftest classifies-invoked-fn-literal-assertions
  (let [findings (analyze "speclj_fn_assertions.clj" 'myapp.fn-assertions-spec #{'myapp.core})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-should-greater-than-as-extroverted
  (is (= :extroverted (:verdict (first (analyze "speclj_should_gt.clj"
                                                 'myapp.should-gt-spec #{'myapp.core}))))))

(deftest classifies-stub-assertions-from-preceding-sut-call
  (let [findings (analyze "speclj_stub_assertions.clj" 'myapp.stub-assertions-spec #{'myapp.core})]
    (is (= 2 (count findings)))
    (is (every? #(= :extroverted (:verdict %)) findings))))

(deftest classifies-assertions-inside-conditionals
  (let [findings (analyze "conditional_assertions.clj"
                          'myapp.conditional-spec
                          #{'myapp.core})]
    (is (= 5 (count findings)))
    (is (every? #(pos? (count (get-in % [:trace :assertions]))) findings))
    (is (= 1 (count (filter #(= :conditional-assertion (:verdict %)) findings))))
    (is (= :would-be-extroverted
           (:reason (first (filter #(= :conditional-assertion (:verdict %)) findings)))))
    (is (= 4 (count (filter #(= :extroverted (:verdict %)) findings))))))

(deftest classifies-when-body-with-refer-all-sut
  (let [findings (analyze "coastline_when_empire_body.clj"
                          'myapp.coastline-when-empire-body-spec
                          #{'myapp.core 'myapp.helpers-test})]
    (is (= 1 (count findings)))
    (is (= :conditional-assertion (:verdict (first findings))))
    (is (= :would-be-likely-extroverted (:reason (first findings))))
    (is (= 2 (count (get-in (first findings) [:trace :assertions]))))))

(deftest unconditional-assertion-outranks-conditional
  (let [{:keys [trace verdict]} (first (analyze "conditional_mixed.clj"
                                                'myapp.conditional-mixed-spec
                                                #{'myapp.core}))]
    (is (= :extroverted verdict))
    (is (= 1 (count (:assertions trace))))
    (is (= :extroverted (:verdict (first (:assertions trace)))))))

(deftest edn-findings-include-assertion-trace
  (let [{:keys [trace]} (first (analyze "extroverted_direct.clj"
                                        'myapp.core-test #{'myapp.core}))]
    (is (= 'myapp.core-test (:test-ns trace)))
    (is (contains? (:requires trace) 'myapp.core))
    (is (seq (:assertions trace)))
    (is (= :extroverted (:verdict (first (:assertions trace)))))))

(deftest classifies-case-branch-assertion
  (let [case-body (list 'let '[x (myapp.core/calculate-total [1])]
                        (list 'case 'x
                              :none '(is false)
                              '(is (= x x))))
        forms [(list 'ns 'myapp.case-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'case-branch case-body)]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.case-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :conditional-assertion (:verdict (first findings))))
    (is (= :would-be-extroverted (:reason (first findings))))))

(deftest classifies-introverted-runtime-when-as-conditional
  (let [forms [(list 'ns 'myapp.intro-cond-test
                      (list :require '[clojure.test :refer [deftest is]]))
               (list 'deftest 'intro-in-when
                     (list 'when 'flag '(is (= 1 (count items)))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.intro-cond-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :conditional-assertion (:verdict (first findings))))
    (is (= :no-sut-assertion (:reason (first findings))))))

(deftest classifies-questionable-runtime-when-as-conditional
  (let [forms [(list 'ns 'myapp.quest-cond-test
                      (list :require '[clojure.test :refer [deftest is]]))
               (list 'deftest 'quest-in-when
                     (list 'when 'flag '(expect= 1 2)))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.quest-cond-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :conditional-assertion (:verdict (first findings))))
    (is (= :unknown-assertion-macro (:reason (first findings))))))

(deftest classifies-flattenable-doseq-as-extroverted
  (let [forms [(list 'ns 'myapp.table-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'table-driven
                     (list 'doseq '[n [1 2]]
                           (list 'is (list '= 'n (list 'core/calculate-total (list 'n))))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.table-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-guarded-doseq-as-extroverted
  (let [forms [(list 'ns 'myapp.guarded-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'guarded-table
                     (list 'let '[rows [[1] [2]]]
                           (list 'is (list 'seq 'rows))
                           (list 'doseq '[row rows]
                                 (list 'is (list '= 1 (list 'core/calculate-total 'row))))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.guarded-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-dispatch-if-as-extroverted
  (let [forms [(list 'ns 'myapp.dispatch-if-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'both-branches-assert
                     (list 'if 'expected
                           (list 'is (list '= 1 (list 'core/calculate-total '[1])))
                           (list 'is (list '= 2 (list 'core/calculate-total '[1 2])))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.dispatch-if-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-literal-when-false-as-introverted
  (let [forms [(list 'ns 'myapp.when-false-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'skipped-body
                     (list 'when false
                           (list 'is (list '= 1 (list 'core/calculate-total '[1])))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.when-false-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :introverted (:verdict (first findings))))
    (is (= :no-assertions (:reason (first findings))))))

(deftest classifies-literal-if-true-as-extroverted
  (let [forms [(list 'ns 'myapp.if-true-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'then-branch
                     (list 'if true
                           (list 'is (list '= 1 (list 'core/calculate-total '[1])))
                           (list 'is (list '= 0 (list 'count '[1])))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.if-true-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-not-empty-guard-before-doseq-as-extroverted
  (let [forms [(list 'ns 'myapp.not-empty-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'not-empty-guard
                     (list 'let '[rows [[1]]]
                           (list 'is (list 'not-empty 'rows))
                           (list 'doseq '[row rows]
                                 (list 'is (list '= 1 (list 'core/calculate-total 'row))))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.not-empty-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-literal-dotimes-as-extroverted
  (let [forms [(list 'ns 'myapp.dotimes-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'fixed-count
                     (list 'dotimes '[i 2]
                           (list 'is (list '= 1 (list 'core/calculate-total (list 'inc 'i))))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.dotimes-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-literal-cond-true-as-extroverted
  (let [forms [(list 'ns 'myapp.cond-true-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'first-branch
                     (list 'cond
                           true (list 'is (list '= 1 (list 'core/calculate-total '[1])))
                           :else (list 'is (list '= 0 (list 'count '[1])))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.cond-true-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-empty-guard-before-doseq-as-extroverted
  (let [forms [(list 'ns 'myapp.empty-guard-test
                      (list :require '[speclj.core :refer [describe it should-not should=]]
                            '[myapp.core :as core]))
               (list 'describe 'empty-guard
                     (list 'it 'guarded-table
                           (list 'let '[rows [[1]]]
                                 (list 'should-not (list 'empty? 'rows))
                                 (list 'doseq '[row rows]
                                       (list 'should= 1 (list 'core/calculate-total 'row))))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.empty-guard-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-should-be-nil-guard-before-doseq-as-extroverted
  (let [forms [(list 'ns 'myapp.nil-guard-test
                      (list :require '[speclj.core :refer [describe it should-be-nil should=]]
                            '[myapp.core :as core]))
               (list 'describe 'nil-guard
                     (list 'it 'guarded-table
                           (list 'let '[rows nil]
                                 (list 'should-be-nil 'rows)
                                 (list 'doseq '[row rows]
                                       (list 'should= 0 (list 'core/calculate-total 'row))))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.nil-guard-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-literal-when-not-false-as-extroverted
  (let [forms [(list 'ns 'myapp.when-not-false-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'active-body
                     (list 'when-not false
                           (list 'is (list '= 1 (list 'core/calculate-total '[1])))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.when-not-false-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-literal-when-not-true-as-introverted
  (let [forms [(list 'ns 'myapp.when-not-true-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'skipped-body
                     (list 'when-not true
                           (list 'is (list '= 1 (list 'core/calculate-total '[1])))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.when-not-true-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :introverted (:verdict (first findings))))
    (is (= :no-assertions (:reason (first findings))))))

(deftest classifies-literal-if-false-as-extroverted
  (let [forms [(list 'ns 'myapp.if-false-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'else-branch
                     (list 'if false
                           (list 'is (list '= 0 (list 'count '[1])))
                           (list 'is (list '= 1 (list 'core/calculate-total '[1])))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.if-false-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-runtime-cond-as-conditional
  (let [forms [(list 'ns 'myapp.runtime-cond-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'runtime-branches
                     (list 'let '[x 1]
                           (list 'cond
                                 'x (list 'is (list '= 1 (list 'core/calculate-total '[1])))
                                 :else (list 'is (list '= 0 (list 'count '[1]))))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.runtime-cond-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :conditional-assertion (:verdict (first findings))))))

(deftest classifies-dispatch-if-with-one-unasserted-branch-as-conditional
  (let [forms [(list 'ns 'myapp.partial-dispatch-if-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'one-branch-asserts
                     (list 'if 'flag
                           (list 'is (list '= 1 (list 'core/calculate-total '[1])))
                           '(println "skip")))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.partial-dispatch-if-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :conditional-assertion (:verdict (first findings))))))

(deftest classifies-describe-local-defn-helper-as-extroverted
  (let [forms [(list 'ns 'myapp.helper-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'describe 'helpers
                     (list 'defn 'with-setup '[f]
                           (list 'f 42))
                     (list 'it 'uses-local-helper
                           (list 'with-setup
                                 (list 'fn '[n]
                                       (list 'is (list '= 1 (list 'core/calculate-total (list 'n))))))))]
        finding (first (analyze/analyze-forms forms
                                              {:sut (sut-for 'myapp.helper-test #{'myapp.core})
                                               :project-ctx project-ctx}))]
    (is (= :extroverted (:verdict finding)))))

(deftest classifies-nested-describe-helpers-as-extroverted
  (let [forms [(list 'ns 'myapp.nested-helper-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'describe 'outer
                     (list 'defn 'outer-setup '[f]
                           (list 'f 42))
                     (list 'describe 'inner
                           (list 'it 'uses-outer-helper
                                 (list 'outer-setup
                                       (list 'fn '[n]
                                             (list 'is (list '= 1 (list 'core/calculate-total (list 'n)))))))))]
        finding (first (analyze/analyze-forms forms
                                              {:sut (sut-for 'myapp.nested-helper-test #{'myapp.core})
                                               :project-ctx project-ctx}))]
    (is (= :extroverted (:verdict finding)))))

(deftest reports-missing-doseq-guard-cause
  (let [forms [(list 'ns 'myapp.unguarded-doseq-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'unguarded-table
                     (list 'doseq '[row rows]
                           (list 'is (list '= 1 (list 'core/calculate-total 'row)))))]
        finding (first (analyze/analyze-forms forms
                                              {:sut (sut-for 'myapp.unguarded-doseq-test #{'myapp.core})
                                               :project-ctx project-ctx}))]
    (is (= :conditional-assertion (:verdict finding)))
    (is (= :missing-doseq-guard (:conditional-cause finding)))
    (is (= {:head 'doseq :coll 'rows} (:conditional-context finding)))
    (is (= :missing-doseq-guard
           (get-in finding [:trace :assertions 0 :conditional-cause])))))

(deftest reports-runtime-conditional-cause
  (let [forms [(list 'ns 'myapp.runtime-when-cause-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'runtime-when
                     (list 'when 'flag
                           (list 'is (list '= 1 (list 'core/calculate-total '[1])))))]
        finding (first (analyze/analyze-forms forms
                                              {:sut (sut-for 'myapp.runtime-when-cause-test #{'myapp.core})
                                               :project-ctx project-ctx}))]
    (is (= :conditional-assertion (:verdict finding)))
    (is (= :runtime-conditional (:conditional-cause finding)))
    (is (= {:head 'when} (:conditional-context finding)))))

(deftest reports-partial-dispatch-if-cause
  (let [forms [(list 'ns 'myapp.partial-if-cause-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'one-branch
                     (list 'if 'flag
                           (list 'is (list '= 1 (list 'core/calculate-total '[1])))
                           '(println "skip")))]
        finding (first (analyze/analyze-forms forms
                                              {:sut (sut-for 'myapp.partial-if-cause-test #{'myapp.core})
                                               :project-ctx project-ctx}))]
    (is (= :conditional-assertion (:verdict finding)))
    (is (= :partial-dispatch-if (:conditional-cause finding)))))

(deftest reports-runtime-dotimes-cause
  (let [forms [(list 'ns 'myapp.runtime-dotimes-cause-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'dynamic-count
                     (list 'let '[n 2]
                           (list 'dotimes '[i 'n]
                                 (list 'is (list '= 1 (list 'core/calculate-total (list 'inc 'i)))))))]
        finding (first (analyze/analyze-forms forms
                                              {:sut (sut-for 'myapp.runtime-dotimes-cause-test #{'myapp.core})
                                               :project-ctx project-ctx}))]
    (is (= :conditional-assertion (:verdict finding)))
    (is (= :runtime-dotimes (:conditional-cause finding)))))

(deftest classifies-runtime-dotimes-as-conditional
  (let [forms [(list 'ns 'myapp.runtime-dotimes-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'dynamic-count
                     (list 'let '[n 2]
                           (list 'dotimes '[i 'n]
                                 (list 'is (list '= 1 (list 'core/calculate-total (list 'inc 'i)))))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.runtime-dotimes-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :conditional-assertion (:verdict (first findings))))))

(deftest classifies-large-literal-dotimes-as-conditional
  (let [forms [(list 'ns 'myapp.large-dotimes-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'too-many-iterations
                     (list 'dotimes '[i 33]
                           (list 'is (list '= 1 (list 'core/calculate-total (list 'inc 'i))))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.large-dotimes-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :conditional-assertion (:verdict (first findings))))))

(deftest classifies-do-branch-dispatch-if-as-extroverted
  (let [forms [(list 'ns 'myapp.do-dispatch-if-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'do-and-assert
                     (list 'if 'flag
                           (list 'do
                                 (list 'is (list '= 1 (list 'core/calculate-total '[1])))
                                 '(println "setup"))
                           (list 'is (list '= 2 (list 'core/calculate-total '[1 2])))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.do-dispatch-if-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-literal-cond-skips-false-branches-as-extroverted
  (let [forms [(list 'ns 'myapp.cond-skip-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'skip-to-true
                     (list 'cond
                           false (list 'is (list '= 0 (list 'count '[1])))
                           false (list 'is (list '= 0 (list 'count '[2])))
                           true (list 'is (list '= 1 (list 'core/calculate-total '[1])))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.cond-skip-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-literal-cond-else-only-as-extroverted
  (let [forms [(list 'ns 'myapp.cond-else-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'fallback-branch
                     (list 'cond
                           :else (list 'is (list '= 1 (list 'core/calculate-total '[1])))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.cond-else-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-doseq-case-literal-dispatch-as-extroverted
  (let [forms [(list 'ns 'myapp.case-doseq-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'table-case
                     (list 'doseq '[[args expected]
                                    [[[] :missing-source]
                                     [["bad"] "error message"]]]
                           (list 'let '[result (list 'core/validate-args 'args)]
                                 (list 'case 'expected
                                       :missing-source
                                       (list 'is (list 'contains? 'result ':error))
                                       (list 'is (list '= 'expected (list ':error 'result)))))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.case-doseq-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-case-literal-binding-as-extroverted
  (let [forms [(list 'ns 'myapp.case-bound-test
                      (list :require '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'bound-dispatch
                     (list 'let '[expected :missing-source
                                  result (list 'core/validate-args '[])]
                           (list 'case 'expected
                                 :missing-source
                                 (list 'is (list 'contains? 'result ':error))
                                 (list 'is false))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.case-bound-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))