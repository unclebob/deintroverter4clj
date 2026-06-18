(ns deintroverter.project
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [deintroverter.parse :as parse])
  (:import [java.io File]))

(defn find-project-root
  "Walk up from file-or-dir path until deps.edn is found. Returns path string or nil."
  [start-path]
  (loop [^File dir (if (.isDirectory (io/file start-path))
                     (io/file start-path)
                     (.getParentFile (io/file start-path)))]
    (cond
      (nil? dir) nil
      (.exists (io/file dir "deps.edn")) (.getPath dir)
      :else (recur (.getParentFile dir)))))

(defn- ns-from-file [^File f]
  (try
    (some-> f slurp parse/read-string-all first parse/parse-ns-form :namespace)
    (catch Exception _ nil)))

(defn- clojure-source? [^File f]
  (let [n (.getName f)]
    (or (.endsWith n ".clj")
        (.endsWith n ".cljs")
        (.endsWith n ".cljc"))))

(defn- scan-paths-for-namespaces [root-path path-entries]
  (into #{}
        (for [entry path-entries
              :let [dir (io/file root-path entry)]
              :when (.exists dir)
              f (file-seq dir)
              :when (.isFile ^File f)
              :when (clojure-source? f)
              :let [ns-sym (ns-from-file f)]
              :when ns-sym]
          ns-sym)))

(defn- external-dep-keys [deps-edn]
  (into #{} (filter symbol? (keys deps-edn))))

(defn load-context
  "Load project context from a root path containing deps.edn.
  Returns {:root :in-project-namespaces :external-dep-symbols}."
  [root-path]
  (let [deps-file (io/file root-path "deps.edn")
        deps      (when (.exists deps-file) (edn/read-string (slurp deps-file)))
        paths     (or (:paths deps) ["src"])]
    {:root                   root-path
     :in-project-namespaces  (scan-paths-for-namespaces root-path paths)
     :external-dep-symbols   (external-dep-keys (:deps deps))}))