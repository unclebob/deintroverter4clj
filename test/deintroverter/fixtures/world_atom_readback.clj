(ns myapp.world-atom-readback-spec
  (:require [speclj.core :refer [describe it should]]
            [myapp.core :as core]))

(describe "world atom readback"
  (it "promotes assertions that read a let-bound world atom mutated via injected updater"
    (let [world (atom [[{:type :fighter} {:type :destroyer}]])
          ctx {:update-game-map! (core/update-world-fn world)}]
      (core/mark-major-invasion! ctx [0 0] (get-in @world [0 0]))
      (core/mark-major-invasion! ctx [0 1] (get-in @world [0 1]))
      (should (true? (get-in @world [0 0 :major-invasion])))
      (should (true? (get-in @world [0 1 :major-invasion])))))

  (it "stays introverted when the asserted atom was not passed to the sut"
    (let [world (atom [[{:type :fighter}]])
          ctx {:update-game-map! (core/update-world-fn (atom [[{:type :other}]]))}]
      (core/mark-major-invasion! ctx [0 0] (get-in @world [0 0]))
      (should (true? (get-in @world [0 0 :major-invasion]))))))