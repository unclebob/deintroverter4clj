(ns deintroverter.report
  (:require [clojure.pprint :as pprint]))

(defn- summarize-by-reason [findings verdict]
  (let [matched (filter #(= verdict (:verdict %)) findings)]
    (if (empty? matched)
      {:total 0}
      (into {:total (count matched)}
            (map (fn [[reason findings]]
                   [reason (count findings)])
                 (group-by :reason matched))))))

(defn- summarize [findings]
  {:extroverted        (count (filter #(= :extroverted (:verdict %)) findings))
   :likely-extroverted (count (filter #(= :likely-extroverted (:verdict %)) findings))
   :cloistered         (summarize-by-reason findings :cloistered)
   :introverted        (summarize-by-reason findings :introverted)
   :questionable       (summarize-by-reason findings :questionable)})

(defn- report-by-default? [verdict verbose?]
  (or verbose?
      (and (not= :extroverted verdict)
           (not= :likely-extroverted verdict))))

(defn print-human
  [findings verbose?]
  (doseq [{:keys [file line test-name test-form verdict reason]} findings
          :when (report-by-default? verdict verbose?)]
    (println (str file ":" line "  (" (name test-form) " " test-name ")  " verdict))
    (when reason
      (println (str "  reason: " (name reason))))))

(defn build-edn
  [project-root findings errors]
  {:project-root project-root
   :summary      (assoc (summarize findings) :errors (count errors))
   :findings     findings
   :errors       errors})

(defn print-edn
  [project-root findings errors]
  (pprint/pprint (build-edn project-root findings errors)))

(defn exit-code
  [_findings _errors]
  0)