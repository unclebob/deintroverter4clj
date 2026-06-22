(ns deintroverter.golden-support
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [deintroverter.analyze :as analyze]
            [deintroverter.parse :as parse]
            [deintroverter.project :as project]
            [deintroverter.sut :as sut]))

(def fixtures-dir "test/deintroverter/fixtures")

(defn- fixture-files []
  (->> (io/file fixtures-dir)
       file-seq
       (filter #(and (.isFile %)
                     (.endsWith (.getName %) ".clj")
                     (not (str/includes? (.getPath %) "sample-project"))))
       (map #(.getName %))
       sort))

(defn load-edn-resource [path]
  (edn/read-string (slurp path)))

(def golden-overrides
  (load-edn-resource "test/deintroverter/golden_overrides.edn"))

(def project-ctx
  (project/load-context "test/deintroverter/fixtures/sample-project"))

(defn- sut-for-fixture [fixture-name ns-info]
  (or (get golden-overrides fixture-name)
      (sut/infer-sut-namespaces
       {:test-namespace (:namespace ns-info)
        :requires (:requires ns-info)
        :project-ctx project-ctx
        :add #{}
        :remove #{}})))

(defn- evidence-from-trace [trace]
  (or (:side-effect-evidence trace)
      (:direct-assertion-evidence trace)
      (:wiring-evidence trace)
      (:external-dependency-evidence trace)))

(defn- normalize-finding [fixture-name finding]
  (let [assertion (first (:assertions (:trace finding)))]
    (cond-> {:fixture fixture-name
             :test-name (:test-name finding)
             :verdict (:verdict finding)
             :reason (:reason finding)
             :evidence (some-> assertion evidence-from-trace)}
      (:reason-legacy finding) (assoc :reason-legacy (:reason-legacy finding)))))

(defn golden-findings*
  []
  (vec
   (mapcat
    (fn [fixture-name]
      (let [path (str fixtures-dir "/" fixture-name)
            forms (parse/read-string-all (slurp path))
            ns-info (parse/parse-ns-form (first forms))
            sut (sut-for-fixture fixture-name ns-info)
            findings (analyze/analyze-file path {:sut sut :project-ctx project-ctx})]
        (map #(normalize-finding fixture-name %) findings)))
    (fixture-files))))

(defn golden-findings [] (golden-findings*))

(defn write-golden-findings!
  [path]
  (spit path (pr-str (golden-findings*))))