(ns deintroverter.sut
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn- load-denylist []
  (-> "deintroverter/test_lib_denylist.edn"
      io/resource
      slurp
      edn/read-string))

(defn- clojure-suite? [ns-sym]
  (let [s (name ns-sym)]
    (or (= "clojure" (namespace ns-sym))
        (.startsWith s "clojure."))))

(defn- denied-test-lib? [ns-sym denylist]
  (or (contains? denylist ns-sym)
      (when-let [n (namespace ns-sym)]
        (contains? denylist (symbol n)))))

(defn- name-suffix-test-ns? [ns-sym]
  (let [n (name ns-sym)]
    (or (.endsWith n "-spec")
        (.endsWith n "-test"))))

(defn- under-test-path? [rel-path]
  (boolean
   (when rel-path
     (or (re-find #"(^|/)test/" rel-path)
         (re-find #"(^|/)spec/" rel-path)))))

(defn- test-layer-namespace? [ns-sym project-ctx]
  (or (name-suffix-test-ns? ns-sym)
      (under-test-path? (get-in project-ctx [:namespace-paths ns-sym]))))

(defn- convention-candidate [test-ns]
  (let [n (name test-ns)]
    (cond
      (.endsWith n "-test")
      (symbol (namespace test-ns) (subs n 0 (- (count n) 5)))

      (.endsWith n "-spec")
      (symbol (namespace test-ns) (subs n 0 (- (count n) 5)))

      :else nil)))

(defn- eligible? [ns-sym {:keys [project-ctx denylist]}]
  (and (contains? (:in-project-namespaces project-ctx) ns-sym)
       (not (clojure-suite? ns-sym))
       (not (denied-test-lib? ns-sym denylist))
       (not (contains? (:external-dep-symbols project-ctx) ns-sym))))

(defn eligible-for-analysis?
  "True when ns-sym is an in-project namespace, excluding clojure, test libs, and deps."
  [ns-sym project-ctx]
  (eligible? ns-sym {:project-ctx project-ctx :denylist (load-denylist)}))

(defn infer-sut-namespaces
  [{:keys [test-namespace requires project-ctx add remove]}]
  (let [denylist (load-denylist)
        ctx      {:project-ctx project-ctx :denylist denylist}
        candidates (into #{}
                         (concat
                          (keep convention-candidate [test-namespace])
                          requires))]
    (-> (set (filter #(and (eligible? % ctx)
                           (not (test-layer-namespace? % project-ctx)))
                    candidates))
        (into add)
        (set/difference remove))))