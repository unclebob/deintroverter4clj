(ns deintroverter.parse
  (:require [edamame.core :as edamame]))

(declare parse-ns-form)

(defn- base-parse-opts []
  {:line-numbers? true
   :fn true
   :regex true
   :var true
   :deref true
   :quote true})

(defn- auto-resolve-opts [{:keys [namespace aliases]}]
  (when namespace
    {:auto-resolve (assoc (or aliases {}) :current namespace)}))

(defn- ns-info-from-source [s]
  (try
    (let [ns-form (edamame/parse-string s (base-parse-opts))]
      (when (and (seq? ns-form) (= 'ns (first ns-form)))
        (parse-ns-form ns-form)))
    (catch Exception _ nil)))

(defn read-string-all
  "Read all top-level forms from a string. Attaches :line metadata to each form.
  Resolves ::keyword and ::alias/keyword from the file's ns form and :as aliases."
  [s]
  (let [opts (merge (base-parse-opts)
                    (or (auto-resolve-opts (ns-info-from-source s)) {}))]
    (vec (edamame/parse-string-all s opts))))

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

(defn- require-entry->refer [entry]
  (when (vector? entry)
    (let [pairs (partition 2 (rest entry))]
      (some (fn [[k v]]
              (when (= :refer k) v))
            pairs))))

(defn- accumulate-refer [ns-sym refer refer-all refer-syms]
  (cond
    (nil? ns-sym)
    [refer-all refer-syms]

    (= refer :all)
    [(conj refer-all ns-sym) refer-syms]

    (sequential? refer)
    [refer-all
     (into refer-syms (map (fn [s] [s ns-sym]) refer))]

    :else
    [refer-all refer-syms]))

(defn parse-ns-form
  "Extract {:namespace :aliases :requires :refer-all :refer-syms} from an ns form."
  [ns-form]
  (when-not (and (seq? ns-form) (= 'ns (first ns-form)))
    (throw (ex-info "Not an ns form" {:form ns-form})))
  (let [namespace (second ns-form)
        clauses   (drop 2 ns-form)
        require-clause (some #(when (and (seq? %) (= :require (first %))) %) clauses)
        entries (rest (or require-clause []))
        [refer-all refer-syms]
        (reduce (fn [[ra rs] entry]
                  (accumulate-refer (require-entry->ns-sym entry)
                                    (require-entry->refer entry)
                                    ra rs))
                [#{} {}]
                entries)]
    {:namespace namespace
     :aliases   (into {}
                      (keep (fn [e]
                              (when-let [ns-sym (require-entry->ns-sym e)]
                                (when-let [alias (require-entry->alias e)]
                                  [alias ns-sym])))
                            entries))
     :requires  (into #{}
                      (keep require-entry->ns-sym entries))
     :refer-all refer-all
     :refer-syms refer-syms}))