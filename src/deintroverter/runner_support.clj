(ns deintroverter.runner-support)

(def ^:dynamic *exit-fn*
  (fn [code] (System/exit code)))

(defn exit-if-failed!
  "Exit with status 1 when the clojure.test summary has failures or errors."
  [{:keys [fail error]}]
  (when (pos? (+ fail error))
    (*exit-fn* 1)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-06-19T12:53:05.440474-05:00", :module-hash "1346245671", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "-1100512016"} {:id "def/*exit-fn*", :kind "def", :line 3, :end-line 4, :hash "-1993244469"} {:id "defn/exit-if-failed!", :kind "defn", :line 6, :end-line 10, :hash "1226774826"}]}
;; clj-mutate-manifest-end
