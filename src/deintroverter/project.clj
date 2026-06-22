(ns deintroverter.project
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [deintroverter.parse :as parse])
  (:import [java.io File]))

(defn- starting-dir [start-path]
  (let [file (io/file start-path)]
    (if (.isDirectory file) file (.getParentFile file))))

(defn find-project-root
  "Walk up from file-or-dir path until deps.edn is found. Returns path string or nil."
  [start-path]
  (loop [^File dir (starting-dir start-path)]
    (when dir
      (or (when (.exists (io/file dir "deps.edn")) (.getPath dir))
          (recur (.getParentFile dir))))))

(defn- namespace-from-forms [forms]
  (some-> forms first parse/parse-ns-form :namespace))

(defn- ns-from-file [^File f]
  (try
    (let [s (slurp f)]
      (or (parse/namespace-from-source s)
          (namespace-from-forms (parse/read-string-all s))))
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

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-06-21T09:31:48.085519-05:00", :module-hash "-278126889", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 6, :hash "1296794403"} {:id "defn-/starting-dir", :kind "defn-", :line 8, :end-line 10, :hash "-204578920"} {:id "defn/find-project-root", :kind "defn", :line 12, :end-line 18, :hash "-721077089"} {:id "defn-/namespace-from-forms", :kind "defn-", :line 20, :end-line 21, :hash "-489411270"} {:id "defn-/ns-from-file", :kind "defn-", :line 23, :end-line 26, :hash "-1467898751"} {:id "defn-/clojure-source?", :kind "defn-", :line 28, :end-line 32, :hash "-1672845625"} {:id "def/skip-path-segments", :kind "def", :line 34, :end-line 35, :hash "-1451131706"} {:id "defn-/skipped-path?", :kind "defn-", :line 37, :end-line 40, :hash "-1414815561"} {:id "defn-/relative-path", :kind "defn-", :line 42, :end-line 47, :hash "245603369"} {:id "defn-/scan-paths", :kind "defn-", :line 49, :end-line 59, :hash "1923791851"} {:id "defn-/scan-paths-for-namespaces", :kind "defn-", :line 61, :end-line 62, :hash "1580434159"} {:id "defn-/scan-paths-for-namespace-paths", :kind "defn-", :line 64, :end-line 65, :hash "-1079296736"} {:id "defn-/external-dep-keys", :kind "defn-", :line 67, :end-line 68, :hash "-1253492089"} {:id "defn-/alias-extra-paths", :kind "defn-", :line 70, :end-line 73, :hash "311598043"} {:id "defn-/scan-path-entries", :kind "defn-", :line 75, :end-line 77, :hash "365659668"} {:id "defn/load-context", :kind "defn", :line 79, :end-line 90, :hash "1114040893"}]}
;; clj-mutate-manifest-end
