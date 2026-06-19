(ns myapp.core)

(defn calculate-total [items]
  (count items))

(defn split-items [items]
  [(first items) (second items)])

(defn split-pairs [items]
  [[(nth items 0) (nth items 1)] [(nth items 2) (nth items 3)]])

(defn labeled-pair [a b]
  {:a a :b b})

(defn move-coastline-unit [_] nil)

