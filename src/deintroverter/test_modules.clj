(ns deintroverter.test-modules
  (:require [clojure.set :as set]
            [deintroverter.sut :as sut]))

(defn- name-suffix-test-ns? [ns-sym]
  (let [n (name ns-sym)]
    (or (.endsWith n "-spec")
        (.endsWith n "-test"))))

(defn- under-test-path? [rel-path]
  (boolean
   (when rel-path
     (or (re-find #"(^|/)test/" rel-path)
         (re-find #"(^|/)spec/" rel-path)))))

(defn test-layer-namespace?
  "True when ns-sym lives in the test layer (-spec/-test suffix or test/spec path)."
  [ns-sym project-ctx]
  (and (sut/eligible-for-analysis? ns-sym project-ctx)
       (or (name-suffix-test-ns? ns-sym)
           (under-test-path?
            (get-in project-ctx [:namespace-paths ns-sym])))))

(defn test-module-namespace?
  "True when ns-sym is another test-layer namespace per project conventions."
  [ns-sym {:keys [test-namespace sut project-ctx]}]
  (and (test-layer-namespace? ns-sym project-ctx)
       (not= ns-sym test-namespace)
       (not (contains? sut ns-sym))))

(defn infer-test-module-namespaces
  [{:keys [test-namespace requires sut project-ctx]}]
  (into #{}
        (filter #(test-module-namespace?
                 % {:test-namespace test-namespace
                    :sut sut
                    :project-ctx project-ctx})
                (set/union (:in-project-namespaces project-ctx)
                           (or requires #{})))))