(ns myapp.core)

(defn calculate-total [items]
  (count items))

(defn split-items [items]
  [(first items) (second items)])

(defn labeled-pair [a b]
  {:a a :b b})