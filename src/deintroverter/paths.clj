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