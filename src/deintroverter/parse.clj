(ns deintroverter.parse
  (:require [edamame.core :as edamame]))

(declare parse-ns-form)

(defn- base-parse-opts []
  {:line-numbers? true
   :fn true
   :regex true
   :var true
   :deref true
   :quote true
   :syntax-quote true})

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

(defn- quoted-ns-sym? [entry]
  (and (list? entry) (= 'quote (first entry))))

(defn- vector-ns-sym? [entry]
  (and (vector? entry) (symbol? (first entry))))

(defn- require-entry->ns-sym [entry]
  (cond
    (symbol? entry) entry
    (vector-ns-sym? entry) (first entry)
    (quoted-ns-sym? entry) (second entry)
    :else nil))

(defn- require-entry-kw [entry kw]
  (when (vector? entry)
    (some (fn [[k v]] (when (= kw k) v))
          (partition 2 (rest entry)))))

(defn- require-entry->alias [entry]
  (require-entry-kw entry :as))

(defn- require-entry->refer [entry]
  (require-entry-kw entry :refer))

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

(defn- require-entries [ns-form]
  (let [clauses (drop 2 ns-form)
        require-clause (some #(when (and (seq? %) (= :require (first %))) %) clauses)]
    (rest (or require-clause []))))

(defn- refer-state [entries]
  (reduce (fn [[ra rs] entry]
            (accumulate-refer (require-entry->ns-sym entry)
                              (require-entry->refer entry)
                              ra rs))
          [#{} {}]
          entries))

(defn- alias-map [entries]
  (into {}
        (keep (fn [e]
                (when-let [ns-sym (require-entry->ns-sym e)]
                  (when-let [alias (require-entry->alias e)]
                    [alias ns-sym])))
              entries)))

(defn parse-ns-form
  "Extract {:namespace :aliases :requires :refer-all :refer-syms} from an ns form."
  [ns-form]
  (when-not (and (seq? ns-form) (= 'ns (first ns-form)))
    (throw (ex-info "Not an ns form" {:form ns-form})))
  (let [entries (require-entries ns-form)
        [refer-all refer-syms] (refer-state entries)]
    {:namespace (second ns-form)
     :aliases (alias-map entries)
     :requires (into #{} (keep require-entry->ns-sym entries))
     :refer-all refer-all
     :refer-syms refer-syms}))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-06-19T12:52:45.401975-05:00", :module-hash "-911368363", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "1091730279"} {:id "form/1/declare", :kind "declare", :line 4, :end-line 4, :hash "1563402843"} {:id "defn-/base-parse-opts", :kind "defn-", :line 6, :end-line 13, :hash "1801213796"} {:id "defn-/auto-resolve-opts", :kind "defn-", :line 15, :end-line 17, :hash "1280693994"} {:id "defn-/ns-info-from-source", :kind "defn-", :line 19, :end-line 24, :hash "1086377370"} {:id "defn/read-string-all", :kind "defn", :line 26, :end-line 32, :hash "-454909666"} {:id "defn-/quoted-ns-sym?", :kind "defn-", :line 34, :end-line 35, :hash "291464383"} {:id "defn-/vector-ns-sym?", :kind "defn-", :line 37, :end-line 38, :hash "-1592921524"} {:id "defn-/require-entry->ns-sym", :kind "defn-", :line 40, :end-line 45, :hash "1054331939"} {:id "defn-/require-entry-kw", :kind "defn-", :line 47, :end-line 50, :hash "-828074735"} {:id "defn-/require-entry->alias", :kind "defn-", :line 52, :end-line 53, :hash "-2039005386"} {:id "defn-/require-entry->refer", :kind "defn-", :line 55, :end-line 56, :hash "-491479622"} {:id "defn-/accumulate-refer", :kind "defn-", :line 58, :end-line 71, :hash "-717342686"} {:id "defn-/require-entries", :kind "defn-", :line 73, :end-line 76, :hash "-1717154199"} {:id "defn-/refer-state", :kind "defn-", :line 78, :end-line 84, :hash "-563425156"} {:id "defn-/alias-map", :kind "defn-", :line 86, :end-line 92, :hash "388534996"} {:id "defn/parse-ns-form", :kind "defn", :line 94, :end-line 105, :hash "-1286226295"}]}
;; clj-mutate-manifest-end
