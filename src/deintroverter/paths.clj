(ns deintroverter.paths
  (:import [java.io File]))

(def ^:private extensions #{"clj" "cljs" "cljc"})

(def ^:private skip-dir-names #{".git" ".worktrees" "node_modules" "target"})

(defn- skip-dir? [^File dir]
  (contains? skip-dir-names (.getName dir)))

(defn- extension [^File f]
  (let [name (.getName f)
        dot  (.lastIndexOf name ".")]
    (when (pos? dot)
      (.toLowerCase (subs name (inc dot))))))

(defn- clojure-file? [^File f]
  (and (.isFile f) (contains? extensions (extension f))))

(defn- collect-from-dir [^File dir acc]
  (let [children (.listFiles dir)]
    (if (nil? children)
      acc
      (reduce (fn [a ^File child]
                (cond
                  (and (.isDirectory child) (not (skip-dir? child)))
                  (collect-from-dir child a)

                  (clojure-file? child)
                  (conj a child)

                  :else a))
              acc
              (seq children)))))

(defn collect-files
  "Given path strings (files or directories), return a deduped vector of
  File objects for all .clj, .cljs, and .cljc files. Directories are
  scanned recursively."
  [path-strs]
  (->> path-strs
       (mapcat (fn [p]
                 (let [f (File. p)]
                   (cond
                     (.isFile f)      [f]
                     (.isDirectory f) (collect-from-dir f [])
                     :else            []))))
       distinct
       vec))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-06-21T09:31:44.84715-05:00", :module-hash "-1974311130", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "1222603339"} {:id "def/extensions", :kind "def", :line 4, :end-line 4, :hash "1162180089"} {:id "def/skip-dir-names", :kind "def", :line 6, :end-line 6, :hash "539849058"} {:id "defn-/skip-dir?", :kind "defn-", :line 8, :end-line 9, :hash "1974775745"} {:id "defn-/extension", :kind "defn-", :line 11, :end-line 15, :hash "1123721278"} {:id "defn-/clojure-file?", :kind "defn-", :line 17, :end-line 18, :hash "-368301780"} {:id "defn-/collect-from-dir", :kind "defn-", :line 20, :end-line 34, :hash "988314342"} {:id "defn/collect-files", :kind "defn", :line 36, :end-line 49, :hash "-1879090174"}]}
;; clj-mutate-manifest-end
