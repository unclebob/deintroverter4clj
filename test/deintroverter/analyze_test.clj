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
    (is (= :extroverted (:verdict (first findings))))
    (is (= :conditional-assertion (:verdict (second findings))))
    (is (= :extroverted (:verdict (nth findings 2))))))

(deftest classifies-through-setup-forms
  (let [findings (analyze "speclj_setup.clj" 'myapp.setup-spec #{'myapp.core})]
    (is (= 2 (count findings)))
    (is (every? #(= :extroverted (:verdict %)) findings))))

(deftest classifies-invoked-fn-literal-assertions
  (let [findings (analyze "speclj_fn_assertions.clj" 'myapp.fn-assertions-spec #{'myapp.core})]
    (is (= 1 (count findings)))
    (is (= :conditional-assertion (:verdict (first findings))))
    (is (= :would-be-extroverted (:reason (first findings))))))

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
    (is (every? #(= :conditional-assertion (:verdict %)) findings))
    (is (every? #(:conditional? (first (get-in % [:trace :assertions]))) findings))
    (is (= :would-be-extroverted (:reason (first findings))))))

(deftest classifies-when-body-with-refer-all-sut
  (let [findings (analyze "coastline_when_empire_body.clj"
                          'myapp.coastline-when-empire-body-spec
                          #{'myapp.core 'myapp.helpers-test})]
    (is (= 1 (count findings)))
    (is (= :conditional-assertion (:verdict (first findings))))
    (is (= :would-be-likely-extroverted (:reason (first findings))))
    (is (= 2 (count (get-in (first findings) [:trace :assertions]))))))

(deftest unconditional-assertion-outranks-conditional
  (let [{:keys [trace]} (first (analyze "conditional_mixed.clj"
                                        'myapp.conditional-mixed-spec
                                        #{'myapp.core}))]
    (is (= :extroverted (:verdict (first (:assertions trace)))))
    (is (true? (:conditional? (second (:assertions trace)))))
    (is (= :extroverted (:underlying-verdict (second (:assertions trace)))))))

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

(deftest classifies-introverted-conditional-assertion
  (let [forms [(list 'ns 'myapp.intro-cond-test
                      (list :require '[clojure.test :refer [deftest is]]))
               (list 'deftest 'intro-in-when
                     (list 'when true '(is (= 1 (count items)))))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.intro-cond-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :conditional-assertion (:verdict (first findings))))
    (is (= :no-sut-assertion (:reason (first findings))))))

(deftest classifies-questionable-conditional-assertion
  (let [forms [(list 'ns 'myapp.quest-cond-test
                      (list :require '[clojure.test :refer [deftest is]]))
               (list 'deftest 'quest-in-when
                     (list 'when true '(expect= 1 2)))]
        findings (analyze/analyze-forms forms
                                        {:sut (sut-for 'myapp.quest-cond-test #{'myapp.core})
                                         :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :conditional-assertion (:verdict (first findings))))
    (is (= :unknown-assertion-macro (:reason (first findings))))))