(ns myapp.core)

(defonce process-state (atom nil))

(defn process [x]
  (reset! process-state x)
  x)

(defn run-with-handler [handler x]
  (handler x))

(defn calculate-total [items]
  (count items))

(defn split-items [items]
  [(first items) (second items)])

(defn split-pairs [items]
  [[(nth items 0) (nth items 1)] [(nth items 2) (nth items 3)]])

(defn labeled-pair [a b]
  {:a a :b b})

(defn move-coastline-unit [_] nil)

(defn write-text-file [path content]
  (spit path content))

(defn update-world-fn [world]
  (fn [updater]
    (swap! world updater)))

(defn mark-major-invasion! [ctx path contents]
  ((:update-game-map! ctx)
   #(assoc-in % path (assoc contents :major-invasion true))))

