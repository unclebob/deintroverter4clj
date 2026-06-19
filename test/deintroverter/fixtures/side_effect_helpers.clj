(ns myapp.side-effect-spec
  (:require [speclj.core :refer [describe it should=]]
            [myapp.core :as core]
            [myapp.helpers-test :as helpers]))

(describe "side effect grounding"
  (it "promotes when sut precedes test-module read in assertion"
    (helpers/set-test-world! {})
    (core/move-coastline-unit [1 1])
    (should= {:game-map {}} (helpers/read-test-state :game-map)))

  (it "promotes when sut precedes let-bound test-state read"
    (helpers/set-test-world! {})
    (core/move-coastline-unit [1 1])
    (let [state (helpers/read-test-state :game-map)]
      (should= {} state)))

  (it "stays cloistered without sut call"
    (should= true (helpers/valid-input? 1))))