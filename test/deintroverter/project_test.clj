(ns deintroverter.project-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [deintroverter.project :as project]))

(def fixture-root
  (.getPath (io/file "test/deintroverter/fixtures/sample-project")))

(deftest finds-deps-edn-walking-up
  (let [from (.getPath (io/file fixture-root "src/myapp/core.clj"))]
    (is (= fixture-root (project/find-project-root from)))))

(deftest discovers-in-project-namespaces
  (let [ctx (project/load-context fixture-root)]
    (is (contains? (:in-project-namespaces ctx) 'myapp.core))
    (is (contains? (:external-dep-symbols ctx) 'org.clojure/test.check))))