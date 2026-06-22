(ns myapp.reader-conditional
  #?(:cljs (:refer-clojure :exclude [clone]))
  (:require [clojure.spec.alpha :as s #?@(:cljs [:include-macros true])]))

(defn process-events [events]
  events)