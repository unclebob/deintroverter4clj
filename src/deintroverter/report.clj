(ns deintroverter.report
  (:require [clojure.pprint :as pprint]))

(defn- summarize [findings]
  {:extroverted        (count (filter #(= :extroverted (:verdict %)) findings))
   :likely-extroverted (count (filter #(= :likely-extroverted (:verdict %)) findings))
   :introverted        (count (filter #(= :introverted (:verdict %)) findings))
   :questionable       (count (filter #(= :questionable (:verdict %)) findings))})

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
  [findings errors]
  (if (or (seq errors)
          (some #(#{:introverted :questionable} (:verdict %)) findings))
    1
    0))