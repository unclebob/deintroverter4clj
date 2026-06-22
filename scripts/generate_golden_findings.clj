#!/usr/bin/env bb
;; Regenerate committed golden findings snapshot.
(babashka.process/shell "clojure -M:test -e \"(require '[deintroverter.golden-support :as g]) (g/write-golden-findings! \\\"test/deintroverter/golden_findings.edn\\\") (println (count (g/golden-findings*)) \\\"findings\\\")\"")