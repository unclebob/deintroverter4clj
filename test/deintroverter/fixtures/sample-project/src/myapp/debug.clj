(ns myapp.debug)

(defn format-cell [pos cell]
  (str pos ":" (:type cell)))