(ns deintroverter.stack-depth-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.trace :as trace]
            [deintroverter.analyze :as analyze]
            [deintroverter.project :as project]))

(def sut #{'myapp.core})
(def resolve-ns (fn [sym] (if (symbol? sym) (name sym) sym)))
(def trace-ctx (trace/make-trace-ctx
                 {:namespace 'myapp.core-test
                  :requires #{'myapp.core}
                  :refer-syms {}
                  :refer-all #{}}
                 sut
                 resolve-ns))

(defn- deep-list [depth leaf]
  (loop [acc leaf n 0]
    (if (= n depth)
      acc
      (recur (list 'count acc) (inc n)))))

(defn- binding-chain [depth]
  (into {}
        (map (fn [i]
               [(symbol (str "a" i))
                (if (zero? i)
                  '(myapp.core/calculate-total items)
                  (symbol (str "a" (dec i))))])
             (range depth))))

(deftest traces-deeply-nested-forms-without-overflow
  (is (= :introverted
         (:verdict (trace/trace-form (deep-list 4000 42) {} trace-ctx)))))

(deftest traces-long-binding-chains-without-overflow
  (is (= :extroverted
         (:verdict (trace/trace-form 'a999 (binding-chain 1000) trace-ctx)))))

(defn- nested-let-body [depth]
  (loop [acc '(is (= 1 result)) n 0]
    (if (= n depth)
      acc
      (recur (list 'let [(symbol (str "x" n)) n] acc)
             (inc n)))))

(deftest analyzes-deeply-nested-lets-without-overflow
  (let [project-ctx (project/load-context "test/deintroverter/fixtures/sample-project")
        forms [(list 'ns 'myapp.deep-let-test
                      (list :require
                            '[clojure.test :refer [deftest is]]
                            '[myapp.core :as core]))
               (list 'deftest 'deep-lets
                     (list 'let '[result (myapp.core/calculate-total [1])]
                           (nested-let-body 500)))]
        findings (analyze/analyze-forms forms {:sut sut :project-ctx project-ctx})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest analyzes-self-recursive-helper-without-overflow
  (let [project-ctx (project/load-context ".")
        findings (analyze/analyze-file "test/deintroverter/paths_test.clj"
                                       {:sut #{'deintroverter.paths}
                                        :project-ctx project-ctx})]
    (is (= 3 (count findings)))
    (is (every? #(= :extroverted (:verdict %)) findings))))