(ns deintroverter.reasons-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.reasons :as reasons]))

(deftest normalize-reason-maps-legacy-keywords
  (is (= :provenance-link (reasons/normalize-reason :sut-side-effect-heuristic)))
  (is (= :provenance-link (reasons/normalize-reason :sut-direct-assertion-heuristic)))
  (is (= :external-wiring (reasons/normalize-reason :sut-wiring-heuristic)))
  (is (= :external-file (reasons/normalize-reason :file-dependency)))
  (is (= :no-sut-assertion (reasons/normalize-reason :no-sut-assertion))))

(deftest reason-legacy-retains-old-keywords
  (is (= :sut-side-effect-heuristic (reasons/reason-legacy :sut-side-effect-heuristic)))
  (is (nil? (reasons/reason-legacy :provenance-link))))

(deftest provenance-link-result-includes-legacy-alias
  (is (= {:reason :provenance-link :reason-legacy :sut-side-effect-heuristic}
         (reasons/provenance-link-result :sut-side-effect-heuristic))))

(deftest external-result-helpers-include-legacy-aliases
  (is (= {:reason :external-wiring :reason-legacy :sut-wiring-heuristic}
         (reasons/external-wiring-result)))
  (is (= {:reason :external-file :reason-legacy :file-dependency}
         (reasons/external-file-result))))