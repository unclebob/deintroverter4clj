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

(defn infer-sut-namespaces
  [{:keys [test-namespace requires project-ctx add remove]}]
  (let [denylist (load-denylist)
        ctx      {:project-ctx project-ctx :denylist denylist}
        candidates (into #{}
                         (concat
                          (keep convention-candidate [test-namespace])
                          requires))]
    (-> (set (filter #(eligible? % ctx) candidates))
        (into add)
        (set/difference remove))))