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

(deftest analyzes-empire-game-loop-spec-without-overflow
  (let [empire-root "/Users/unclebob/projects/clojure/empire/empire-2025"
        path (str empire-root "/spec/empire/game_loop/item_processing_computer_spec.clj")]
    (when (.exists (java.io.File. path))
      (let [project-ctx (project/load-context empire-root)
            findings (analyze/analyze-file path
                                          {:sut (:in-project-namespaces project-ctx)
                                           :project-ctx project-ctx})]
        (is (pos? (count findings)))))))

(deftest analyzes-deeply-nested-lets-without-overflow
  (let [project-ctx (project/load-context "test/deintroverter/fixtures/sample-project")
        nested (loop [acc '(is (= 1 result)) n 0]
                 (if (= n 500)
                   acc
                   (recur (list 'let [(symbol (str "x" n)) n] acc)
                          (inc n))))
        source (str "(ns myapp.deep-let-test\n"
                    "  (:require [clojure.test :refer [deftest is]]\n"
                    "            [myapp.core :as core]))\n"
                    "(deftest deep-lets\n"
                    "  (let [result (core/calculate-total [1])]\n"
                    nested "))\n")
        f (doto (java.io.File/createTempFile "deep-let" ".clj")
            (.deleteOnExit))]
    (spit f source)
    (let [findings (analyze/analyze-file (.getPath f)
                                         {:sut sut :project-ctx project-ctx})]
      (is (= 1 (count findings)))
      (is (= :extroverted (:verdict (first findings)))))))