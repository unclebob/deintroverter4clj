(ns deintroverter.reasons)

(def ^:private legacy-reasons
  #{:sut-side-effect-heuristic
    :sut-direct-assertion-heuristic
    :sut-wiring-heuristic
    :file-dependency})

(defn normalize-reason
  "Map legacy assertion reason keywords to the normalized taxonomy."
  [reason]
  (case reason
    (:sut-side-effect-heuristic :sut-direct-assertion-heuristic) :provenance-link
    :sut-wiring-heuristic :external-wiring
    :file-dependency :external-file
    reason))

(defn reason-legacy
  "Return the pre-taxonomy reason keyword when reason was normalized."
  [reason]
  (when (contains? legacy-reasons reason) reason))

(defn provenance-link-result
  "Build {:reason :reason-legacy} for provenance-link paths."
  [legacy-reason]
  {:reason :provenance-link
   :reason-legacy legacy-reason})

(defn external-wiring-result []
  {:reason :external-wiring
   :reason-legacy :sut-wiring-heuristic})

(defn external-file-result []
  {:reason :external-file
   :reason-legacy :file-dependency})