(ns myapp.coastline-when-empire-body-spec
  (:require [speclj.core :refer [deftest should=]]
            [myapp.helpers-test :as test-utils]
            [myapp.core :refer [move-coastline-unit]]
            [myapp.helpers-test :refer [build-test-map set-test-unit set-test-player-map! set-test-world!]]))

(deftest wakes-up-when-hitting-map-edge
  (set-test-world! (build-test-map ["~~~"
                                    "#T~"
                                    "#~~"]))
  (set-test-unit (test-utils/game-map-atom) "T"
                 :mode :coastline-follow
                 :coastline-steps 50
                 :start-pos [1 1]
                 :visited #{[1 1]}
                 :prev-pos nil)
  (set-test-player-map! (test-utils/read-test-state :game-map))
  (move-coastline-unit [1 1])
  (let [cell-1-0 (get-in (test-utils/read-test-state :game-map) [1 0])
        cell-2-0 (get-in (test-utils/read-test-state :game-map) [2 0])
        woken-unit (or (:contents cell-1-0) (:contents cell-2-0))]
    (when woken-unit
      (should= :awake (:mode woken-unit))
      (should= :hit-edge (:reason woken-unit)))))