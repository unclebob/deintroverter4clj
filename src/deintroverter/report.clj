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

(defn- summarize-conditional-causes [findings]
  (let [matched (filter #(= :conditional-assertion (:verdict %)) findings)
        with-cause (filter :conditional-cause matched)]
    (when (seq with-cause)
      (into {}
            (map (fn [[cause findings]]
                   [cause (count findings)])
                 (group-by :conditional-cause with-cause))))))

(defn- summarize [findings]
  (let [conditional (summarize-by-reason findings :conditional-assertion)
        by-cause (summarize-conditional-causes findings)]
    {:extroverted            (count (filter #(= :extroverted (:verdict %)) findings))
     :likely-extroverted     (count (filter #(= :likely-extroverted (:verdict %)) findings))
     :conditional-assertion  (cond-> conditional
                                 by-cause (assoc :by-cause by-cause))
     :cloistered             (summarize-by-reason findings :cloistered)
     :introverted            (summarize-by-reason findings :introverted)
     :questionable           (summarize-by-reason findings :questionable)}))

(defn- report-by-default? [verdict verbose?]
  (or verbose?
      (and (not= :extroverted verdict)
           (not= :likely-extroverted verdict))))

(def ^:private conditional-cause-formatters
  {:missing-doseq-guard (fn [{:keys [coll]}] (str "missing doseq guard on " coll))
   :near-doseq-guard (fn [{:keys [coll]}] (str "near doseq guard on " coll))
   :non-flattenable-doseq (fn [{:keys [coll]}] (str "non-flattenable doseq collection " coll))
   :runtime-dotimes (fn [{:keys [count]}] (str "runtime dotimes count " count))
   :partial-dispatch-if (fn [_] "partial dispatch-if (one branch asserts)")
   :runtime-conditional (fn [{:keys [head]}] (str "runtime " (name head)))
   :malformed-doseq (fn [_] "malformed doseq binding")})

(defn- format-conditional-cause [cause context]
  (if-let [formatter (get conditional-cause-formatters cause)]
    (formatter context)
    (name cause)))

(defn print-human
  [findings verbose?]
  (doseq [{:keys [file line test-name test-form verdict reason conditional-cause conditional-context]}
          findings
          :when (report-by-default? verdict verbose?)]
    (println (str file ":" line "  (" (name test-form) " " test-name ")  " verdict))
    (when reason
      (println (str "  reason: " (name reason))))
    (when conditional-cause
      (println (str "  cause: " (format-conditional-cause conditional-cause conditional-context))))))

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
;; {:version 1, :tested-at "2026-06-21T09:47:10.634975-05:00", :module-hash "-162266184", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "486268263"} {:id "defn-/summarize-by-reason", :kind "defn-", :line 4, :end-line 11, :hash "170078755"} {:id "defn-/summarize-conditional-causes", :kind "defn-", :line 13, :end-line 20, :hash "717490825"} {:id "defn-/summarize", :kind "defn-", :line 22, :end-line 31, :hash "-1355570752"} {:id "defn-/report-by-default?", :kind "defn-", :line 33, :end-line 36, :hash "-1788957063"} {:id "defn-/format-conditional-cause", :kind "defn-", :line 38, :end-line 47, :hash "-212862637"} {:id "defn/print-human", :kind "defn", :line 49, :end-line 58, :hash "615033391"} {:id "defn/build-edn", :kind "defn", :line 60, :end-line 65, :hash "-1547937653"} {:id "defn/print-edn", :kind "defn", :line 67, :end-line 69, :hash "1536227182"} {:id "defn/exit-code", :kind "defn", :line 71, :end-line 73, :hash "187940237"}]}
;; clj-mutate-manifest-end
