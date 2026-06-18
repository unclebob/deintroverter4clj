(ns deintroverter.parse
  (:require [edamame.core :as edamame]))

(defn read-string-all
  "Read all top-level forms from a string. Attaches :line metadata to each form."
  [s]
  (vec (edamame/parse-string-all s {:line-numbers? true :fn true :regex true})))

(defn- require-entry->ns-sym [entry]
  (cond
    (symbol? entry) entry
    (and (vector? entry) (symbol? (first entry))) (first entry)
    (and (list? entry) (= 'quote (first entry))) (second entry)
    :else nil))

(defn- require-entry->alias [entry]
  (when (vector? entry)
    (let [pairs (partition 2 (rest entry))]
      (some (fn [[k v]]
              (when (= :as k) v))
            pairs))))

(defn parse-ns-form
  "Extract {:namespace :aliases :requires} from an ns form."
  [ns-form]
  (when-not (and (seq? ns-form) (= 'ns (first ns-form)))
    (throw (ex-info "Not an ns form" {:form ns-form})))
  (let [namespace (second ns-form)
        clauses   (drop 2 ns-form)
        require-clause (some #(when (and (seq? %) (= :require (first %))) %) clauses)
        entries (rest (or require-clause []))]
    {:namespace namespace
     :aliases   (into {}
                      (keep (fn [e]
                              (when-let [ns-sym (require-entry->ns-sym e)]
                                (when-let [alias (require-entry->alias e)]
                                  [alias ns-sym])))
                            entries))
     :requires  (into #{}
                      (keep require-entry->ns-sym entries))}))