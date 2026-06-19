(ns check-parens
  (:require [edamame.core :as edamame]
            [clojure.string :as str]))

(def parse-opts
  {:all true :fn true :quote true :var true :deref true :regex true})

(defn- stack-balance [s]
  (loop [line 1, col 0, stack [], chars (seq s)]
    (if (empty? chars)
      {:ok? (empty? stack), :stack stack}
      (let [c (first chars)
            col (if (= c \newline) 0 (inc col))
            line (if (= c \newline) (inc line) line)]
        (cond
          (= c \;)
          (recur line col stack (drop-while #(not= % \newline) chars))

          (= c \")
          (let [after (drop-while #(not= % \") (rest chars))]
            (recur line (+ col (count after) 1) stack (rest after)))

          (= c \()
          (recur line col (conj stack {:char c :line line :col col}) (rest chars))

          (= c \[)
          (recur line col (conj stack {:char c :line line :col col}) (rest chars))

          (= c \{)
          (recur line col (conj stack {:char c :line line :col col}) (rest chars))

          (= c \))
          (if (and (seq stack) (= \( (:char (peek stack))))
            (recur line col (pop stack) (rest chars))
            {:ok? false :unexpected {:char c :line line :col col} :stack stack})

          (= c \])
          (if (and (seq stack) (= \[ (:char (peek stack))))
            (recur line col (pop stack) (rest chars))
            {:ok? false :unexpected {:char c :line line :col col} :stack stack})

          (= c \})
          (if (and (seq stack) (= \{ (:char (peek stack))))
            (recur line col (pop stack) (rest chars))
            {:ok? false :unexpected {:char c :line line :col col} :stack stack})

          :else
          (recur line col stack (rest chars)))))))

(defn- read-source [path]
  (if (= path "-") (slurp *in*) (slurp path)))

(defn check-file
  "Returns {:ok? bool :path string :message string-or-nil}"
  [path]
  (let [s (read-source path)
        balance (stack-balance s)]
    (try
      (edamame/parse-string-all s parse-opts)
      (if (:ok? balance)
        {:ok? true :path path}
        {:ok? false :path path
         :message (str "Unclosed "
                       (str/join ", " (map #(str (:char %) " at " (:line %) ":" (:col %))
                                           (:stack balance))))})
      (catch Exception e
        {:ok? false :path path
         :message (if (:ok? balance)
                    (.getMessage e)
                    (str (.getMessage e) " — also: unclosed "
                         (str/join ", " (map #(str (:char %) " at " (:line %) ":" (:col %))
                                             (:stack balance)))))}))))

(defn -main [& paths]
  (let [paths (if (seq paths) paths ["-"])
        results (for [p paths
                      :let [r (check-file (if (= p "-") "-" p))]
                      :let [r (assoc r :path (if (= p "-") "<stdin>" p))]]
                  r)]
    (doseq [{:keys [ok? path message]} results]
      (if ok?
        (println "OK" path)
        (println "FAIL" path message)))
    (System/exit (if (every? :ok? results) 0 1))))