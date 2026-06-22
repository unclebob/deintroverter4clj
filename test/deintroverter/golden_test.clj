(ns deintroverter.golden-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [deintroverter.golden-support :as golden]))

(def ^:private golden-path "test/deintroverter/golden_findings.edn")

(defn- load-golden []
  (edn/read-string (slurp golden-path)))

(defn- diff-report [only-expected only-actual]
  (str "Golden findings changed.\n"
       "Only in golden: " (count only-expected) "\n"
       (when (seq only-expected)
         (str "  " (pr-str (take 5 only-expected)) "\n"))
       "Only in live: " (count only-actual) "\n"
       (when (seq only-actual)
         (str "  " (pr-str (take 5 only-actual)) "\n"))
       "Regenerate: bb scripts/generate_golden_findings.clj"))

(deftest golden-findings-unchanged
  (testing "fixture findings match committed golden snapshot"
    (let [expected (set (load-golden))
          actual (set (golden/golden-findings))
          only-expected (seq (clojure.set/difference expected actual))
          only-actual (seq (clojure.set/difference actual expected))]
      (is (and (nil? only-expected) (nil? only-actual))
          (diff-report only-expected only-actual)))))