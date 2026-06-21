(ns myapp.with-redefs-world-readback-spec
  (:require [speclj.core :refer [describe it should]]
            [myapp.core :as core]))

(describe "with-redefs world readback"
  (it "promotes world atom assertions after sut inside with-redefs"
    (let [world (atom [[{:type :fighter}]])
          ctx {:update-game-map! (core/update-world-fn world)}]
      (with-redefs [core/calculate-total (fn [_] 0)]
        (core/mark-major-invasion! ctx [0 0] (get-in @world [0 0])))
      (should (true? (get-in @world [0 0 :major-invasion]))))))