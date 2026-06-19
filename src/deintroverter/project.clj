(ns deintroverter.project
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
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

(def ^:private skip-path-segments #{"/.worktrees/" "/.git/"
                                    "/node_modules/" "/target/"})

(defn- skipped-path? [rel-path]
  (boolean
   (some #(str/includes? rel-path %)
         skip-path-segments)))

(defn- relative-path [root-path ^File f]
  (let [root (.getCanonicalFile (io/file root-path))
        file (.getCanonicalFile f)
        root-prefix (str (.getPath root) java.io.File/separator)]
    (when (.startsWith (.getPath file) root-prefix)
      (subs (.getPath file) (count root-prefix)))))

(defn- scan-paths [root-path path-entries]
  (for [entry path-entries
        :let [dir (io/file root-path entry)]
        :when (.exists dir)
        f (file-seq dir)
        :when (.isFile ^File f)
        :when (clojure-source? f)
        :let [ns-sym (ns-from-file f)
              rel    (relative-path root-path f)]
        :when (and ns-sym rel (not (skipped-path? rel)))]
    {:namespace ns-sym :path rel}))

(defn- scan-paths-for-namespaces [root-path path-entries]
  (into #{} (map :namespace (scan-paths root-path path-entries))))

(defn- scan-paths-for-namespace-paths [root-path path-entries]
  (into {} (map (juxt :namespace :path) (scan-paths root-path path-entries))))

(defn- external-dep-keys [deps-edn]
  (into #{} (filter symbol? (keys deps-edn))))

(defn- alias-extra-paths [deps]
  (mapcat (fn [[_alias-name alias-map]]
            (or (:extra-paths alias-map) []))
          (:aliases deps)))

(defn- scan-path-entries [deps]
  (vec (distinct (concat (or (:paths deps) ["src"])
                         (alias-extra-paths deps)))))

(defn load-context
  "Load project context from a root path containing deps.edn.
  Scans :paths plus :extra-paths from every entry in :aliases.
  Returns {:root :in-project-namespaces :namespace-paths :external-dep-symbols}."
  [root-path]
  (let [deps-file (io/file root-path "deps.edn")
        deps      (when (.exists deps-file) (edn/read-string (slurp deps-file)))
        paths     (scan-path-entries deps)]
    {:root                   root-path
     :in-project-namespaces  (scan-paths-for-namespaces root-path paths)
     :namespace-paths        (scan-paths-for-namespace-paths root-path paths)
     :external-dep-symbols   (external-dep-keys (:deps deps))}))