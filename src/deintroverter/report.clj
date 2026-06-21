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
  {:extroverted            (count (filter #(= :extroverted (:verdict %)) findings))
   :likely-extroverted     (count (filter #(= :likely-extroverted (:verdict %)) findings))
   :conditional-assertion  (summarize-by-reason findings :conditional-assertion)
   :cloistered             (summarize-by-reason findings :cloistered)
   :introverted            (summarize-by-reason findings :introverted)
   :questionable           (summarize-by-reason findings :questionable)})

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

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-06-21T09:31:51.306637-05:00", :module-hash "-1783862519", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "486268263"} {:id "defn-/summarize-by-reason", :kind "defn-", :line 4, :end-line 11, :hash "170078755"} {:id "defn-/summarize", :kind "defn-", :line 13, :end-line 19, :hash "-766439982"} {:id "defn-/report-by-default?", :kind "defn-", :line 21, :end-line 24, :hash "-1788957063"} {:id "defn/print-human", :kind "defn", :line 26, :end-line 32, :hash "1172584755"} {:id "defn/build-edn", :kind "defn", :line 34, :end-line 39, :hash "-1547937653"} {:id "defn/print-edn", :kind "defn", :line 41, :end-line 43, :hash "1536227182"} {:id "defn/exit-code", :kind "defn", :line 45, :end-line 47, :hash "187940237"}]}
;; clj-mutate-manifest-end
