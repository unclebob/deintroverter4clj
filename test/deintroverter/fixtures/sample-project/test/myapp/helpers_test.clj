(ns myapp.helpers-test)

(defn valid-input? [x]
  (pos? x))

(defn set-world! [] nil)

(defn build-test-map [_] {})

(defn set-test-unit [_ _ & _] nil)

(defn set-test-player-map! [_] nil)

(defn set-test-world! [_] nil)

(defn read-test-state [_]
  {:game-map {}})

(def game-map-atom (atom {}))