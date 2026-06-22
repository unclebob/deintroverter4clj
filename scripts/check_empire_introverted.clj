#!/usr/bin/env bb
;; Exit 1 when empire scan reports introverted findings.
;; Usage: PROJECT_ROOT=/path/to/empire/empire-2025 bb scripts/check_empire_introverted.clj
(when-not (System/getenv "PROJECT_ROOT")
  (throw (ex-info "Set PROJECT_ROOT to the empire project root" {})))

(let [out (with-out-str (babashka.process/shell "bb scripts/categorize_introverted.clj"))]
  (print out)
  (let [introverted-count (or (some->> out
                                       (re-find #"empire-2025 introverted: (\d+)")
                                       second
                                       parse-long)
                              0)]
    (when (pos? introverted-count)
      (println)
      (println "FAIL: introverted count must be 0, got" introverted-count)
      (System/exit 1))
    (println "OK: empire introverted count is 0")))