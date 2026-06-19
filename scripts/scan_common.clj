(ns scan-common
  "Project-scan helpers for external scripts. Not part of deintroverter."
  (:require [clojure.string :as str]
            [deintroverter.sut :as sut]
            [deintroverter.test-modules :as test-modules])
  (:import [java.io File]))

(def skip-dir-names #{".git" ".worktrees" "node_modules" "target"
                       "generated-acceptance-specs" "acceptance"})

(def ^:private acceptance-path-segments
  #{"/acceptance/" "/generated-acceptance-specs/"})

(def ^:private acceptance-scan-paths #{"generated-acceptance-specs"})

(defn acceptance-test-path?
  [path]
  (let [p (str/replace path "\\" "/")]
    (boolean
     (or (some #(str/includes? p %) acceptance-path-segments)
         (re-find #"(^|/)generated-acceptance-specs(/|$)" p)))))

(defn acceptance-scan-path?
  [path-entry]
  (contains? acceptance-scan-paths path-entry))

(defn skip-directory?
  [^File dir]
  (contains? skip-dir-names (.getName dir)))

(defn spec-test-file?
  [^File f]
  (and (.isFile f)
       (let [n (.getName f)]
         (or (.endsWith n "_spec.clj") (.endsWith n "_test.clj")))
       (not (acceptance-test-path? (.getPath f)))))

(defn scan-path-entries
  [deps]
  (let [extra (mapcat (fn [[_ alias-map]]
                        (or (:extra-paths alias-map) []))
                      (:aliases deps))]
    (vec (distinct
          (remove acceptance-scan-path?
                  (concat (or (:paths deps) ["src"]) extra))))))

(defn- ns-from-file [^File f]
  (try (some-> f slurp read-string second) (catch Throwable _ nil)))

(defn clojure-source-file?
  [^File f]
  (and (.isFile f)
       (let [n (.getName f)]
         (or (.endsWith n ".clj")
             (.endsWith n ".cljs")
             (.endsWith n ".cljc")))))

(defn discover-sut-namespaces
  "Augment per-file SUT inference with production namespaces from src/.
  Unlike a naive src/ walk, excludes test-layer namespaces so test helpers
  (e.g. empire.test.utils) remain test modules for cloistered detection."
  [project-root project-ctx]
  (into #{}
        (filter #(and (sut/eligible-for-analysis? % project-ctx)
                      (not (test-modules/test-layer-namespace? % project-ctx)))
                (keep ns-from-file
                      (filter clojure-source-file?
                              (file-seq (File. project-root "src")))))))

(defn rel-path
  [project-root file]
  (str/replace file (str project-root "/") ""))

(defn categorize-finding
  [project-root {:keys [file test-name reason]}]
  (let [rel (rel-path project-root file)
        n   (str/lower-case (str test-name " " rel))]
    (cond
      (= :no-assertions reason)
      :no-parsed-assertions

      (str/includes? rel "architecture/")
      :test-tooling

      (or (str/includes? rel "save_load")
          (str/includes? rel "application/")
          (str/includes? rel "init_spec"))
      :app-lifecycle-io

      (or (str/includes? rel "atoms")
          (str/includes? rel "debug_logging"))
      :global-state-atoms

      (or (str/includes? rel "cache")
          (re-find #"cached|cache " n))
      :memoization-caches

      (or (str/includes? rel "ui/")
          (str/includes? rel "map_render")
          (str/includes? rel "messages_draw")
          (str/includes? rel "rendering"))
      :ui-rendering

      (or (str/includes? rel "player/commands")
          (str/includes? rel "player/orders")
          (str/includes? rel "player/attention")
          (str/includes? rel "player/production")
          (str/includes? rel "player/movement"))
      :player-command-layer

      (or (str/includes? rel "game-loop")
          (str/includes? rel "game_loop")
          (str/includes? rel "item_processing")
          (str/includes? rel "round_setup")
          (str/includes? rel "round-start"))
      :game-loop-orchestration

      (or (str/includes? rel "threat_response")
          (str/includes? rel "major_invasion")
          (str/includes? rel "probe")
          (re-find #"reveal" n))
      :threat-response-invasion

      (or (str/includes? rel "transport")
          (str/includes? rel "loading_manifest")
          (str/includes? rel "sailing"))
      :transport-pipeline

      (or (str/includes? rel "army_movement")
          (str/includes? rel "army_combat")
          (str/includes? rel "army_positioning")
          (str/includes? rel "army_coastal")
          (str/includes? rel "kamikazee")
          (re-find #"registry|country.id|staging" n))
      :army-movement-combat

      (or (str/includes? rel "fighter")
          (str/includes? rel "patrol_boat")
          (str/includes? rel "ship/")
          (str/includes? rel "production/decisions")
          (str/includes? rel "early_game"))
      :computer-unit-behavior

      (or (str/includes? rel "coastline")
          (str/includes? rel "pathfinding")
          (str/includes? rel "movement/")
          (str/includes? rel "visibility"))
      :movement-geometry-visibility

      (str/includes? rel "containers/")
      :container-ops

      :else
      :other-production-mechanics)))