# deintroverter Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a Babashka CLI that statically analyzes `clojure.test` and Speclj files, classifying each test as `:extroverted`, `:introverted`, or `:questionable` based on whether assertions trace to SUT function calls.

**Architecture:** Syntax-tree walk with edamame parsing. Pipeline: resolve input paths → discover project via `deps.edn` → infer SUT namespaces → parse test files → trace asserted values (direct, `let`, `->`, `->>`) → report findings. No macro expansion.

**Tech Stack:** Babashka, Clojure, edamame, clojure.test (for self-tests)

---

## File Map

| File | Responsibility |
|---|---|
| `bb.edn` | Dependencies, `test` and `deintroverter` tasks |
| `src/deintroverter/paths.clj` | Expand file/dir args to `.clj`/`.cljs`/`.cljc` paths |
| `src/deintroverter/parse.clj` | edamame read, `ns` form parsing, form metadata (line numbers) |
| `src/deintroverter/project.clj` | Find `deps.edn`, read `:paths`, scan in-project namespaces, external dep keys |
| `src/deintroverter/sut.clj` | Build SUT namespace set from convention, requires, CLI overrides, exclusions |
| `src/deintroverter/trace.clj` | Desugar `->`/`->>`, collect calls, trace values to SUT |
| `src/deintroverter/assertions.clj` | Recognize assertion macros, extract asserted value forms |
| `src/deintroverter/analyze.clj` | Find `deftest`/`it` bodies, walk forms, produce per-test verdicts |
| `src/deintroverter/report.clj` | Human + EDN output, summary, exit code |
| `src/deintroverter/core.clj` | CLI arg parsing, orchestration |
| `src/deintroverter/test_runner.clj` | Run all tests under Babashka |
| `test/deintroverter/*_test.clj` | Unit tests per namespace |
| `test/deintroverter/fixtures/**` | Sample test files for integration tests |

---

### Task 1: Project scaffolding

**Files:**
- Create: `bb.edn`
- Create: `src/deintroverter/test_runner.clj`
- Create: `test/deintroverter/smoke_test.clj`

- [ ] **Step 1: Write the failing smoke test**

```clojure
(ns deintroverter.smoke-test
  (:require [clojure.test :refer [deftest is testing]]))

(deftest project-loads
  (is (string? "deintroverter")))
```

- [ ] **Step 2: Run test to verify it fails (no runner yet)**

Run: `bb test`
Expected: FAIL — task `test` not found or runner missing

- [ ] **Step 3: Create `bb.edn`**

```clojure
{:paths ["src"]
 :deps {borkdude/edamame {:mvn/version "1.4.27"}}
 :tasks
 {test {:extra-paths ["test"]
        :exec-fn deintroverter.test-runner/run}
  deintroverter {:main-opts ["-m" "deintroverter.core"]}}}
```

- [ ] **Step 4: Create test runner**

```clojure
(ns deintroverter.test-runner
  (:require [clojure.test :refer [run-tests]]
            deintroverter.smoke-test))

(defn run [_]
  (let [{:keys [fail error]} (run-tests 'deintroverter.smoke-test)]
    (when (pos? (+ fail error))
      (System/exit 1))))
```

- [ ] **Step 5: Run test to verify it passes**

Run: `bb test`
Expected: PASS (1 test, 0 failures)

- [ ] **Step 6: Commit**

```bash
git add bb.edn src/deintroverter/test_runner.clj test/deintroverter/smoke_test.clj
git commit -m "chore: scaffold babashka project with test runner"
```

---

### Task 2: Path resolution

**Files:**
- Create: `src/deintroverter/paths.clj`
- Create: `test/deintroverter/paths_test.clj`
- Modify: `src/deintroverter/test_runner.clj`

- [ ] **Step 1: Write the failing test**

```clojure
(ns deintroverter.paths-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [deintroverter.paths :as paths]))

(defn- tmp-dir []
  (doto (io/file (System/getProperty "java.io.tmpdir")
                  (str "deintroverter-" (random-uuid)))
    .mkdirs))

(defn- write-file [dir name content]
  (let [f (io/file dir name)]
    (.mkdirs (.getParentFile f))
    (spit f content)
    f))

(deftest collects-clojure-files-recursively
  (let [dir (tmp-dir)]
    (try
      (write-file dir "src/a.clj" "(ns a)")
      (write-file dir "src/sub/b.cljc" "(ns b)")
      (write-file dir "src/sub/skip.txt" "nope")
      (write-file dir "nested/deep/c.cljs" "(ns c)")
      (is (= #{"a.clj" "b.cljc" "c.cljs"}
             (set (map #(.getName %) (paths/collect-files [(.getPath dir)])))))
      (finally
        (.delete (io/file dir "src/sub/b.cljc"))
        (.delete (io/file dir "src/sub"))
        (.delete (io/file dir "src/a.clj"))
        (.delete (io/file dir "src"))
        (.delete (io/file dir "nested/deep/c.cljs"))
        (.delete (io/file dir "nested/deep"))
        (.delete (io/file dir "nested"))
        (.delete dir)))))

(deftest accepts-single-file
  (let [dir (tmp-dir)
        f   (write-file dir "one.clj" "(ns one)")]
    (try
      (is (= [f] (paths/collect-files [(.getPath f)])))
      (finally (.delete f) (.delete dir)))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bb test`
Expected: FAIL — namespace `deintroverter.paths` not found

- [ ] **Step 3: Implement `paths.clj`**

```clojure
(ns deintroverter.paths
  (:import [java.io File]))

(def ^:private extensions #{"clj" "cljs" "cljc"})

(defn- clojure-file? [^File f]
  (and (.isFile f)
       (extensions (.toLowerCase (subs (.getName f)
                                       (max 0 (dec (.lastIndexOf (.getName f) "."))))))))

(defn- collect-from-dir [^File dir acc]
  (let [children (.listFiles dir)]
    (if (nil? children)
      acc
      (reduce (fn [a ^File child]
                (cond
                  (.isDirectory child) (collect-from-dir child a)
                  (clojure-file? child)  (conj a child)
                  :else a))
              acc
              (seq children)))))

(defn collect-files
  "Given path strings (files or directories), return a deduped vector of
  File objects for all .clj, .cljs, and .cljc files. Directories are
  scanned recursively."
  [path-strs]
  (->> path-strs
       (mapcat (fn [p]
                 (let [f (File. p)]
                   (cond
                     (.isFile f)       [f]
                     (.isDirectory f)  (collect-from-dir f [])
                     :else             []))))
       distinct
       vec))
```

- [ ] **Step 4: Register test namespace in runner**

Add `deintroverter.paths-test` to the `run-tests` call in `test_runner.clj`.

- [ ] **Step 5: Run test to verify it passes**

Run: `bb test`
Expected: PASS

- [ ] **Step 6: Commit**

```bash
git add src/deintroverter/paths.clj test/deintroverter/paths_test.clj src/deintroverter/test_runner.clj
git commit -m "feat: add recursive clojure file path collection"
```

---

### Task 3: Parsing helpers

**Files:**
- Create: `src/deintroverter/parse.clj`
- Create: `test/deintroverter/parse_test.clj`
- Modify: `src/deintroverter/test_runner.clj`

- [ ] **Step 1: Write the failing test**

```clojure
(ns deintroverter.parse-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.parse :as parse]))

(def sample-ns-form
  '(ns myapp.core-test
     (:require [clojure.test :refer [deftest is]]
               [myapp.core :as core])))

(deftest reads-forms-with-metadata
  (let [forms (parse/read-string-all
               (str "(ns myapp.core-test)\n"
                    "(deftest t (is true))"))]
    (is (= 2 (count forms)))
    (is (= 'myapp.core-test (second (first forms))))))

(deftest parses-ns-requires-and-aliases
  (let [{:keys [namespace aliases requires]} (parse/parse-ns-form sample-ns-form)]
    (is (= 'myapp.core-test namespace))
    (is (= 'core (get aliases 'myapp.core)))
    (is (= '#{clojure.test myapp.core} (set requires)))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bb test`
Expected: FAIL — `deintroverter.parse` not found

- [ ] **Step 3: Implement `parse.clj`**

```clojure
(ns deintroverter.parse
  (:require [edamame.core :as edamame]))

(defn read-string-all
  "Read all top-level forms from a string. Attaches :line metadata to each form."
  [s]
  (let [reader (edamame/parse-string s {:all :true})]
    (loop [forms []]
      (if-let [form (edamame/next reader)]
        (recur (conj forms form))
        forms))))

(defn- require-entry->ns-sym [entry]
  (cond
    (symbol? entry) entry
    (and (vector? entry) (symbol? (first entry))) (first entry)
    (and (list? entry) (= 'quote (first entry))) (second entry)
    :else nil))

(defn- require-entry->alias [entry]
  (when (vector? entry)
    (some (fn [[k v]]
            (when (= :as k) v))
          (rest entry))))

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
```

- [ ] **Step 4: Run test to verify it passes**

Run: `bb test`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/deintroverter/parse.clj test/deintroverter/parse_test.clj src/deintroverter/test_runner.clj
git commit -m "feat: add edamame parsing and ns form extraction"
```

---

### Task 4: Project context (`deps.edn`)

**Files:**
- Create: `src/deintroverter/project.clj`
- Create: `test/deintroverter/project_test.clj`
- Create: `test/deintroverter/fixtures/sample-project/deps.edn`
- Create: `test/deintroverter/fixtures/sample-project/src/myapp/core.clj`
- Modify: `src/deintroverter/test_runner.clj`

- [ ] **Step 1: Create fixture project**

`test/deintroverter/fixtures/sample-project/deps.edn`:
```clojure
{:paths ["src"]
 :deps {org.clojure/test.check {:mvn/version "1.1.1"}}}
```

`test/deintroverter/fixtures/sample-project/src/myapp/core.clj`:
```clojure
(ns myapp.core
  (defn calculate-total [items] (count items)))
```

- [ ] **Step 2: Write the failing test**

```clojure
(ns deintroverter.project-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [deintroverter.project :as project]))

(def fixture-root
  (.getPath (io/file "test/deintroverter/fixtures/sample-project")))

(deftest finds-deps-edn-walking-up
  (let [from (.getPath (io/file fixture-root "src/myapp/core.clj"))]
    (is (= fixture-root (project/find-project-root from)))))

(deftest discovers-in-project-namespaces
  (let [ctx (project/load-context fixture-root)]
    (is (contains? (:in-project-namespaces ctx) 'myapp.core))
    (is (contains? (:external-dep-symbols ctx) 'org.clojure/test.check))))
```

- [ ] **Step 3: Run test to verify it fails**

Run: `bb test`
Expected: FAIL — `deintroverter.project` not found

- [ ] **Step 4: Implement `project.clj`**

```clojure
(ns deintroverter.project
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [deintroverter.parse :as parse])
  (:import [java.io File]))

(defn find-project-root
  "Walk up from file-or-dir path until deps.edn is found. Returns path string or nil."
  [start-path]
  (loop [^File dir (if (.isDirectory (io/file start-path))
                     (io/file start-path)
                     (.getParentFile (io/file start-path)))]
    (cond
      (nil? dir) nil
      (.exists (io/file dir "deps.edn")) (.getPath dir)
      :else (recur (.getParentFile dir)))))

(defn- ns-from-file [^File f]
  (try
    (some-> f slurp parse/read-string-all first parse/parse-ns-form :namespace)
    (catch Exception _ nil)))

(defn- scan-paths-for-namespaces [root-path path-entries]
  (into #{}
        (comp
         (map #(io/file root-path %))
         (mapcat (fn [^File dir]
                   (when (.exists dir)
                     (file-seq dir))))
         (filter #(.isFile ^File %))
         (filter #(let [n (.getName ^File %)]
                    (or (.endsWith n ".clj")
                        (.endsWith n ".cljs")
                        (.endsWith n ".cljc"))))
         (map ns-from-file)
         (filter some?)))

(defn- external-dep-keys [deps-edn]
  (into #{}
        (comp
         (map key)
         (filter symbol?))))

(defn load-context
  "Load project context from a root path containing deps.edn.
  Returns {:root :in-project-namespaces :external-dep-symbols}."
  [root-path]
  (let [deps-file (io/file root-path "deps.edn")
        deps      (when (.exists deps-file) (edn/read-string (slurp deps-file)))
        paths     (or (:paths deps) ["src"])]
    {:root                   root-path
     :in-project-namespaces  (scan-paths-for-namespaces root-path paths)
     :external-dep-symbols (external-dep-keys (:deps deps))}))
```

- [ ] **Step 5: Run test to verify it passes**

Run: `bb test`
Expected: PASS

- [ ] **Step 6: Commit**

```bash
git add src/deintroverter/project.clj test/deintroverter/project_test.clj test/deintroverter/fixtures/
git commit -m "feat: discover project root and in-project namespaces from deps.edn"
```

---

### Task 5: SUT inference

**Files:**
- Create: `src/deintroverter/sut.clj`
- Create: `resources/deintroverter/test_lib_denylist.edn`
- Create: `test/deintroverter/sut_test.clj`
- Modify: `src/deintroverter/test_runner.clj`

- [ ] **Step 1: Create denylist resource**

`resources/deintroverter/test_lib_denylist.edn`:
```clojure
#{speclj.core speclj.spec-test clojure.test.check test.check
  bond.james shrubbery.core}
```

- [ ] **Step 2: Write the failing test**

```clojure
(ns deintroverter.sut-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.sut :as sut]))

(def project-ctx
  {:in-project-namespaces #{'myapp.core 'myapp.helpers}
   :external-dep-symbols #{'org.clojure/test.check}})

(deftest convention-strips-test-suffix
  (is (= #{'myapp.core}
         (sut/infer-sut-namespaces
          {:test-namespace 'myapp.core-test
           :requires #{}
           :project-ctx project-ctx
           :add #{} :remove #{}}))))

(deftest excludes-clojure-and-test-libs
  (let [sut-ns (sut/infer-sut-namespaces
                {:test-namespace 'myapp.core-test
                 :requires #{'clojure.test 'speclj.core 'myapp.core}
                 :project-ctx project-ctx
                 :add #{} :remove #{}})]
    (is (contains? sut-ns 'myapp.core))
    (is (not (contains? sut-ns 'clojure.test)))
    (is (not (contains? sut-ns 'speclj.core)))))

(deftest cli-add-and-remove-overrides
  (is (= #{'myapp.extra}
         (sut/infer-sut-namespaces
          {:test-namespace 'myapp.core-test
           :requires #{'myapp.core}
           :project-ctx project-ctx
           :add #{'myapp.extra}
           :remove #{'myapp.core}}))))
```

- [ ] **Step 3: Run test to verify it fails**

Run: `bb test`
Expected: FAIL

- [ ] **Step 4: Implement `sut.clj`**

```clojure
(ns deintroverter.sut
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn- load-denylist []
  (-> "deintroverter/test_lib_denylist.edn"
      io/resource
      slurp
      edn/read-string))

(defn- clojure-suite? [ns-sym]
  (let [s (name ns-sym)]
    (or (= "clojure" (namespace ns-sym))
        (.startsWith s "clojure."))))

(defn- denied-test-lib? [ns-sym denylist]
  (or (contains? denylist ns-sym)
      (when-let [n (namespace ns-sym)]
        (contains? denylist (symbol n)))))

(defn- convention-candidate [test-ns]
  (let [n (name test-ns)]
    (when (.endsWith n "-test")
      (symbol (namespace test-ns)
              (subs n 0 (- (count n) 5))))))

(defn- eligible? [ns-sym {:keys [project-ctx denylist]}]
  (and (contains? (:in-project-namespaces project-ctx) ns-sym)
       (not (clojure-suite? ns-sym))
       (not (denied-test-lib? ns-sym denylist))
       (not (contains? (:external-dep-symbols project-ctx) ns-sym))))

(defn infer-sut-namespaces
  [{:keys [test-namespace requires project-ctx add remove]}]
  (let [denylist (load-denylist)
        ctx      (assoc project-ctx :denylist denylist)
        candidates (into #{}
                         (concat
                          (keep #(convention-candidate test-namespace) [test-namespace])
                          requires))]
    (->> candidates
         (filter #(eligible? % ctx))
         (into #{}
               (concat add))
         (set/difference remove))))
```

Note: add `(require '[clojure.set :as set])` at top of `sut.clj`.

- [ ] **Step 5: Update `bb.edn` paths to include resources**

```clojure
{:paths ["src" "resources"]
 ...}
```

- [ ] **Step 6: Run test to verify it passes**

Run: `bb test`
Expected: PASS

- [ ] **Step 7: Commit**

```bash
git add src/deintroverter/sut.clj resources/ test/deintroverter/sut_test.clj bb.edn
git commit -m "feat: infer SUT namespaces with exclusions and CLI overrides"
```

---

### Task 6: Value tracing

**Files:**
- Create: `src/deintroverter/trace.clj`
- Create: `test/deintroverter/trace_test.clj`
- Modify: `src/deintroverter/test_runner.clj`

- [ ] **Step 1: Write the failing test**

```clojure
(ns deintroverter.trace-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.trace :as trace]))

(def sut #{'myapp.core})
(def resolve-ns (fn [sym] (namespace sym)))

(deftest direct-sut-call-is-extroverted
  (is (= {:verdict :extroverted :reason nil}
         (trace/trace-form '(myapp.core/calculate-total items)
                           {}
                           {:sut sut :resolve-ns resolve-ns}))))

(deftest let-binding-to-sut-call-is-extroverted
  (is (= {:verdict :extroverted :reason nil}
         (trace/trace-form 'result
                           {'result '(myapp.core/calculate-total items)}
                           {:sut sut :resolve-ns resolve-ns}))))

(deftest thread-first-desugars-to-sut
  (is (= {:verdict :extroverted :reason nil}
         (trace/trace-form
          '(-> items (myapp.core/calculate-total) (myapp.core/format))
          {}
          {:sut sut :resolve-ns resolve-ns}))))

(deftest non-sut-only-is-introverted
  (is (= {:verdict :introverted :reason :no-sut-assertion}
         (trace/trace-form '(count items) {} {:sut sut :resolve-ns resolve-ns}))))

(deftest destructuring-is-questionable
  (is (= {:verdict :questionable :reason :destructuring}
         (trace/trace-form 'x
                           {'[a b] '[1 2]}
                           {:sut sut :resolve-ns resolve-ns}))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bb test`
Expected: FAIL

- [ ] **Step 3: Implement `trace.clj`**

```clojure
(ns deintroverter.trace
  (:require [clojure.set :as set]))

(defn- call-sym? [form]
  (and (seq? form) (not= 'quote (first form)) (symbol? (first form))))

(defn- resolve-call-ns [call resolve-ns]
  (when (call-sym? call)
    (let [f (first call)]
      (cond
        (namespace f) (namespace f)
        :else (resolve-ns f)))))

(defn- sut-call? [form {:keys [sut resolve-ns]}]
  (when (call-sym? form)
    (let [ns-s (resolve-call-ns form resolve-ns)]
      (when (and ns-s (contains? sut (symbol ns-s)))
        true))))

(defn- collect-calls [form]
  (cond
    (not (seq? form)) #{}
    (call-sym? form)  (into #{form} (mapcat collect-calls (rest form)))
    :else             (into #{} (mapcat collect-calls form))))

(defn- desugar-> [forms]
  (loop [value (second forms) steps (drop 2 forms)]
    (if (empty? steps)
      value
      (let [step (first steps)]
        (recur (if (and (seq? step) (not= '. (first step)))
                 (cons (first step) (cons value (rest step)))
                 (list step value))
               (rest steps))))))

(defn- desugar->> [forms]
  (loop [value (second forms) steps (drop 2 steps)]
    (if (empty? steps)
      value
      (let [step (first steps)]
        (recur (if (and (seq? step) (not= '. (first step)))
                 (let [args (rest step)]
                   (cons (first step) (concat args [value])))
                 (list step value))
               (rest steps))))))

(defn- expand-threading [form]
  (cond
    (and (seq? form) (= '-> (first form)))  (expand-threading (desugar-> form))
    (and (seq? form) (= '->> (first form))) (expand-threading (desugar->> form))
    :else form))

(defn- binding-destructuring? [binding]
  (or (and (vector? binding) (some vector? binding))
      (and (vector? binding) (some seq? binding))))

(defn trace-form
  "Trace a form to determine assertion verdict.
  bindings: map of symbol → originating form from let.
  ctx: {:sut #{ns-syms} :resolve-ns fn}
  Returns {:verdict :extroverted|:introverted|:questionable :reason keyword-or-nil}"
  [form bindings ctx]
  (cond
    (contains? bindings form)
    (trace-form (get bindings form) bindings ctx)

    (and (symbol? form) (contains? bindings form))
    (trace-form (get bindings form) bindings ctx)

    (and (seq? form) (#{'as-> 'some-> 'some->> 'cond->} (first form)))
    {:verdict :questionable :reason :unsupported-threading-macro}

    (and (seq? form) (= 'fn (first form)))
    {:verdict :questionable :reason :anonymous-fn}

    :else
    (let [expanded (expand-threading form)
          calls    (collect-calls expanded)]
      (cond
        (some #(sut-call? % ctx) calls)
        {:verdict :extroverted :reason nil}

        (some binding-destructuring? (keys bindings))
        {:verdict :questionable :reason :destructuring}

        :else
        {:verdict :introverted :reason :no-sut-assertion}))))
```

- [ ] **Step 4: Run test to verify it passes**

Run: `bb test`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/deintroverter/trace.clj test/deintroverter/trace_test.clj
git commit -m "feat: trace asserted values through let and threading macros"
```

---

### Task 7: Assertion recognition

**Files:**
- Create: `src/deintroverter/assertions.clj`
- Create: `test/deintroverter/assertions_test.clj`
- Modify: `src/deintroverter/test_runner.clj`

- [ ] **Step 1: Write the failing test**

```clojure
(ns deintroverter.assertions-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.assertions :as assertions]))

(deftest recognizes-is-with-equals
  (is (= {:macro :is :asserted-form 'result :reason nil}
         (assertions/parse-assertion '(is (= 42 result)))))

(deftest recognizes-should=
  (is (= {:macro :should= :asserted-form 'actual :reason nil}
         (assertions/parse-assertion '(should= actual expected)))))

(deftest unknown-macro-is-questionable
  (is (= {:macro nil :asserted-form nil :reason :unknown-assertion-macro}
         (assertions/parse-assertion '(assert-custom x)))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bb test`
Expected: FAIL

- [ ] **Step 3: Implement `assertions.clj`**

```clojure
(ns deintroverter.assertions)

(def ^:private known
  '{is :is are :are
    should= :should= should== :should==
    should-be :should-be should-not :should-not
    should-not-be :should-not-be
    should-throw? :should-throw? should-not-throw? :should-not-throw?})

(defn- unquote [form]
  (if (and (seq? form) (= 'quote (first form)))
    (second form)
    form))

(defn- asserted-from-is [body]
  (let [form (unquote body)]
    (cond
      (and (seq? form) (= '= (first form))) (second form)
      :else form)))

(defn- asserted-from-should= [args]
  (first args))

(defn- asserted-from-should-be [args]
  (first args))

(defn parse-assertion
  "Returns {:macro keyword|:nil :asserted-form form|:nil :reason keyword|:nil}"
  [form]
  (when (seq? form)
    (let [mac (first form)
          kw  (get known mac)]
      (cond
        (nil? kw)
        {:macro nil :asserted-form nil :reason :unknown-assertion-macro}

        (= :is kw)
        {:macro :is :asserted-form (asserted-from-is (second form)) :reason nil}

        (= :are kw)
        {:macro :are :asserted-form (second form) :reason nil}

        (#{:should= :should== :should-not} kw)
        {:macro kw :asserted-form (asserted-from-should= (rest form)) :reason nil}

        (#{:should-be :should-not-be} kw)
        {:macro kw :asserted-form (asserted-from-should-be (rest form)) :reason nil}

        (#{:should-throw? :should-not-throw?} kw)
        {:macro kw :asserted-form (second form) :reason nil}

        :else
        {:macro kw :asserted-form (second form) :reason nil}))))
```

- [ ] **Step 4: Run test to verify it passes**

Run: `bb test`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/deintroverter/assertions.clj test/deintroverter/assertions_test.clj
git commit -m "feat: recognize clojure.test and speclj assertion forms"
```

---

### Task 8: File analyzer

**Files:**
- Create: `src/deintroverter/analyze.clj`
- Create: `test/deintroverter/fixtures/extroverted_direct.clj`
- Create: `test/deintroverter/fixtures/introverted_literal.clj`
- Create: `test/deintroverter/fixtures/questionable_destructure.clj`
- Create: `test/deintroverter/analyze_test.clj`
- Modify: `src/deintroverter/test_runner.clj`

- [ ] **Step 1: Create fixture files**

`test/deintroverter/fixtures/extroverted_direct.clj`:
```clojure
(ns myapp.core-test
  (:require [clojure.test :refer [deftest is]]
            [myapp.core :as core]))

(deftest calculates-total
  (is (= 2 (core/calculate-total [1 2]))))
```

`test/deintroverter/fixtures/introverted_literal.clj`:
```clojure
(ns myapp.core-test
  (:require [clojure.test :refer [deftest is]]))

(deftest only-checks-input
  (let [items [1 2 3]]
    (is (= 3 (count items)))))
```

`test/deintroverter/fixtures/questionable_destructure.clj`:
```clojure
(ns myapp.core-test
  (:require [clojure.test :refer [deftest is]]
            [myapp.core :as core]))

(deftest destructures-result
  (let [[a b] (core/split-items [1 2])]
    (is (= 1 a))))
```

- [ ] **Step 2: Write the failing test**

```clojure
(ns deintroverter.analyze-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [deintroverter.analyze :as analyze]
            [deintroverter.project :as project]
            [deintroverter.sut :as sut]))

(defn- fixture [name]
  (.getPath (io/file "test/deintroverter/fixtures" name)))

(def project-ctx
  (project/load-context "test/deintroverter/fixtures/sample-project"))

(defn- sut-for [test-ns requires]
  (sut/infer-sut-namespaces
   {:test-namespace test-ns :requires requires
    :project-ctx project-ctx :add #{} :remove #{}}))

(deftest classifies-extroverted-deftest
  (let [findings (analyze/analyze-file (fixture "extroverted_direct.clj")
                                       {:sut (sut-for 'myapp.core-test #{'myapp.core})})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-introverted-deftest
  (let [findings (analyze/analyze-file (fixture "introverted_literal.clj")
                                       {:sut (sut-for 'myapp.core-test #{})})]
    (is (= :introverted (:verdict (first findings)))))

(deftest classifies-questionable-destructure
  (let [findings (analyze/analyze-file (fixture "questionable_destructure.clj")
                                       {:sut (sut-for 'myapp.core-test #{'myapp.core})})]
    (is (= :questionable (:verdict (first findings))))))
```

- [ ] **Step 3: Run test to verify it fails**

Run: `bb test`
Expected: FAIL

- [ ] **Step 4: Implement `analyze.clj`**

```clojure
(ns deintroverter.analyze
  (:require [deintroverter.parse :as parse]
            [deintroverter.assertions :as assertions]
            [deintroverter.trace :as trace]))

(defn- resolve-ns-fn [{:keys [namespace aliases]}]
  (fn [sym]
    (if-let [n (namespace sym)]
      n
      (some-> (get aliases (symbol (namespace sym))) name)
      (name namespace))))

(defn- test-forms [forms]
  (keep (fn [form]
          (when (seq? form)
            (case (first form)
              deftest {:form :deftest :name (second form) :body (drop 2 form)
                       :line (:line (meta form))}
              it     {:form :it :name (second form) :body (drop 2 form)
                       :line (:line (meta form))}
              nil)))
        forms))

(defn- collect-lets [body bindings]
  (reduce
   (fn [bindings form]
     (if (and (seq? form) (= 'let (first form)))
       (let [pairs (partition 2 (rest form))
             new-b (reduce (fn [b [k v]]
                             (assoc b k v))
                           bindings pairs)]
         (into new-b (map #(collect-lets [%] new-b) (drop (+ 1 (count pairs)) form))))
       bindings))
   bindings
   body))

(defn- assertion-results [body bindings ns-info sut]
  (mapcat
   (fn [form]
     (if-let [{:keys [asserted-form reason]} (assertions/parse-assertion form)]
       (if reason
         [{:verdict :questionable :reason reason}]
         [(trace/trace-form asserted-form bindings
                            {:sut sut :resolve-ns (resolve-ns-fn ns-info)})])
       (when (seq? form)
         (let [inner (rest form)]
           (mapcat #(assertion-results [%] bindings ns-info sut) inner)))))
   body))

(defn- test-verdict [assertion-results]
  (cond
    (some #(= :extroverted (:verdict %)) assertion-results)
    {:verdict :extroverted :reason nil}

    (some #(= :questionable (:verdict %)) assertion-results)
    {:verdict :questionable
     :reason (or (:reason (first (filter #(= :questionable (:verdict %)) assertion-results)))
                 :unknown)}

    (empty? assertion-results)
    {:verdict :introverted :reason :no-assertions}

    :else
    {:verdict :introverted :reason :no-sut-assertion}))

(defn analyze-file
  "Analyze a test file path. opts: {:sut #{namespace-syms}}
  Returns vector of finding maps."
  [file-path {:keys [sut]}]
  (let [content (slurp file-path)
        forms   (parse/read-string-all content)
        ns-form (first forms)
        ns-info (parse/parse-ns-form ns-form)
        tests   (test-forms forms)]
    (vec
     (for [{:keys [form name body line]} tests
           :let [bindings (collect-lets body {})
                 results  (vec (assertion-results body bindings ns-info sut))
                 {:keys [verdict reason]} (test-verdict results)]]
       {:file file-path
        :line line
        :test-name (if (string? name) name (name name))
        :test-form form
        :verdict verdict
        :reason reason
        :sut-namespaces sut}))))
```

- [ ] **Step 5: Run test to verify it passes**

Run: `bb test`
Expected: PASS

- [ ] **Step 6: Commit**

```bash
git add src/deintroverter/analyze.clj test/deintroverter/analyze_test.clj test/deintroverter/fixtures/
git commit -m "feat: analyze deftest and it forms for introverted tests"
```

---

### Task 9: Reporter

**Files:**
- Create: `src/deintroverter/report.clj`
- Create: `test/deintroverter/report_test.clj`
- Modify: `src/deintroverter/test_runner.clj`

- [ ] **Step 1: Write the failing test**

```clojure
(ns deintroverter.report-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.report :as report]))

(def sample-findings
  [{:file "t.clj" :line 10 :test-name "a" :test-form :deftest
    :verdict :introverted :reason :no-sut-assertion :sut-namespaces #{'myapp.core}}
   {:file "t.clj" :line 20 :test-name "b" :test-form :it
    :verdict :extroverted :reason nil :sut-namespaces #{'myapp.core}}])

(deftest human-output-hides-extroverted-by-default
  (let [out (with-out-str (report/print-human sample-findings false))]
    (is (re-find #"introverted" out))
    (is (not (re-find #"extroverted" out)))))

(deftest exit-code-1-when-introverted-or-questionable
  (is (= 1 (report/exit-code sample-findings [])))
  (is (= 0 (report/exit-code [{:verdict :extroverted}] []))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bb test`
Expected: FAIL

- [ ] **Step 3: Implement `report.clj`**

```clojure
(ns deintroverter.report
  (:require [clojure.pprint :as pprint]))

(defn- summarize [findings]
  {:extroverted  (count (filter #(= :extroverted (:verdict %)) findings))
   :introverted  (count (filter #(= :introverted (:verdict %)) findings))
   :questionable (count (filter #(= :questionable (:verdict %)) findings))})

(defn print-human
  [findings verbose?]
  (doseq [{:keys [file line test-name test-form verdict reason]} findings
          :when (or verbose? (not= :extroverted verdict))]
    (println (str file ":" line "  (" test-form " " test-name ")  " verdict))
    (when reason
      (println (str "  reason: " (name reason))))))

(defn build-edn
  [project-root findings errors]
  {:project-root project-root
   :summary      (assoc (summarize findings) :errors (count errors))
   :findings     findings
   :errors       errors})

(defn print-edn
  [project-root findings errors]
  (pprint/pprint (build-edn project-root findings errors)))

(defn exit-code
  [findings errors]
  (if (or (seq errors)
          (some #(#{:introverted :questionable} (:verdict %)) findings))
    1
    0))
```

- [ ] **Step 4: Run test to verify it passes**

Run: `bb test`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/deintroverter/report.clj test/deintroverter/report_test.clj
git commit -m "feat: add human and EDN reporting with exit codes"
```

---

### Task 10: CLI orchestration

**Files:**
- Create: `src/deintroverter/core.clj`
- Create: `test/deintroverter/core_test.clj`
- Modify: `src/deintroverter/test_runner.clj`

- [ ] **Step 1: Write the failing integration test**

```clojure
(ns deintroverter.core-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [deintroverter.core :as core]))

(deftest cli-flags-introverted-fixture
  (let [fixture (.getPath (io/file "test/deintroverter/fixtures/introverted_literal.clj"))
        {:keys [exit findings]} (core/run!
                                 {:paths [fixture]
                                  :project-root "test/deintroverter/fixtures/sample-project"
                                  :format :human
                                  :verbose false
                                  :add-sut #{}
                                  :remove-sut #{}})]
    (is (= 1 exit))
    (is (pos? (count findings)))
    (is (every? #{:introverted :questionable} (set (map :verdict findings))))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bb test`
Expected: FAIL

- [ ] **Step 3: Implement `core.clj`**

```clojure
(ns deintroverter.core
  (:require [deintroverter.paths :as paths]
            [deintroverter.project :as project]
            [deintroverter.sut :as sut]
            [deintroverter.analyze :as analyze]
            [deintroverter.report :as report]
            [deintroverter.parse :as parse])
  (:gen-class))

(defn- parse-args [args]
  (loop [m    {:format :human :verbose false :add-sut #{} :remove-sut #{}
               :project-root nil :paths []}
         args args]
    (if (empty? args)
      m
      (let [a (first args)]
        (cond
          (= "--format" a)        (recur (assoc m :format (keyword (second args)))
                                          (drop 2 args))
          (= "--verbose" a)       (recur (assoc m :verbose true) (rest args))
          (= "--project-root" a)    (recur (assoc m :project-root (second args))
                                          (drop 2 args))
          (= "--sut-ns" a)        (recur (update m :add-sut conj (symbol (second args)))
                                          (drop 2 args))
          (= "--exclude-ns" a)    (recur (update m :remove-sut conj (symbol (second args)))
                                          (drop 2 args))
          (.startsWith a "-")     (throw (ex-info "Unknown option" {:opt a}))
          :else                   (recur (update m :paths conj a) (rest args)))))))

(defn run!
  [{:keys [paths project-root format verbose add-sut remove-sut]}]
  (let [files    (paths/collect-files paths)
        root     (or project-root
                     (some #(project/find-project-root (.getPath %)) files))
        ctx      (when root (project/load-context root))
        findings (vec
                  (mapcat
                   (fn [f]
                     (try
                       (let [content (slurp f)
                             ns-form (first (parse/read-string-all content))
                             ns-info (parse/parse-ns-form ns-form)
                             sut     (sut/infer-sut-namespaces
                                      {:test-namespace (:namespace ns-info)
                                       :requires       (:requires ns-info)
                                       :project-ctx    (or ctx {:in-project-namespaces #{}
                                                                :external-dep-symbols #{}})
                                       :add            add-sut
                                       :remove         remove-sut})]
                         (analyze/analyze-file (.getPath f) {:sut sut}))
                       (catch Exception e
                         [{:error {:type :parse-error :file (.getPath f)
                                   :message (.getMessage e)}}]))))
                   files))
        errors   (vec (keep :error findings))
        tests    (vec (remove :error findings))
        exit     (report/exit-code tests errors)]
    (case format
      :edn  (report/print-edn root tests errors)
      (report/print-human tests verbose))
    {:exit exit :findings tests :errors errors}))

(defn -main [& args]
  (let [{:keys [exit]} (run! (parse-args args))]
    (System/exit exit)))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `bb test`
Expected: PASS

- [ ] **Step 5: Smoke-test CLI manually**

Run: `bb run deintroverter test/deintroverter/fixtures/introverted_literal.clj --project-root test/deintroverter/fixtures/sample-project`
Expected: Human report with `:introverted`, exit code 1

- [ ] **Step 6: Commit**

```bash
git add src/deintroverter/core.clj test/deintroverter/core_test.clj
git commit -m "feat: add CLI entry point orchestrating full analysis pipeline"
```

---

### Task 11: Speclj fixture and framework parity test

**Files:**
- Create: `test/deintroverter/fixtures/speclj_extroverted.clj`
- Create: `test/deintroverter/speclj_test.clj`
- Modify: `src/deintroverter/test_runner.clj`

- [ ] **Step 1: Create Speclj fixture**

```clojure
(ns myapp.core-spec
  (:require [speclj.core :refer [describe it should=]]
            [myapp.core :as core]))

(describe "calculate-total"
  (it "returns count"
    (should= 2 (core/calculate-total [1 2]))))
```

- [ ] **Step 2: Write failing test**

```clojure
(ns deintroverter.speclj-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [deintroverter.core :as core]))

(deftest analyzes-speclj-it-form
  (let [fixture (.getPath (io/file "test/deintroverter/fixtures/speclj_extroverted.clj"))
        {:keys [findings]} (core/run!
                            {:paths [fixture]
                             :project-root "test/deintroverter/fixtures/sample-project"
                             :format :human :verbose true
                             :add-sut #{} :remove-sut #{}})]
    (is (some #(and (= :it (:test-form %)) (= :extroverted (:verdict %))) findings))))
```

- [ ] **Step 3: Run test — fix any gaps in analyzer for Speclj**

Run: `bb test`
Expected: PASS (adjust `analyze.clj` if `it` forms inside `describe` are not found at top level — may need recursive walk)

If `it` forms are nested, update `test-forms` in `analyze.clj` to walk all forms recursively:

```clojure
(defn- walk-forms [forms acc]
  (reduce
   (fn [a form]
     (if (and (seq? form) (#{deftest it} (first form)))
       (conj a {:form (first form) :name (second form) :body (drop 2 form)
                :line (:line (meta form))})
       (if (seq? form)
         (walk-forms (rest form) a)
         a)))
   acc
   forms))
```

Replace `test-forms` call with `(walk-forms forms [])`.

- [ ] **Step 4: Commit**

```bash
git add test/deintroverter/fixtures/speclj_extroverted.clj test/deintroverter/speclj_test.clj src/deintroverter/analyze.clj
git commit -m "feat: support nested speclj it forms"
```

---

### Task 12: EDN output and verbose flags integration test

**Files:**
- Modify: `test/deintroverter/core_test.clj`

- [ ] **Step 1: Add EDN format test**

```clojure
(deftest cli-edn-format
  (let [fixture (.getPath (io/file "test/deintroverter/fixtures/extroverted_direct.clj"))
        out     (with-out-str
                  (core/run!
                   {:paths [fixture]
                    :project-root "test/deintroverter/fixtures/sample-project"
                    :format :edn :verbose true
                    :add-sut #{} :remove-sut #{}}))]
    (is (re-find #":findings" out))
    (is (re-find #":extroverted" out))))
```

- [ ] **Step 2: Run test**

Run: `bb test`
Expected: PASS

- [ ] **Step 3: Commit**

```bash
git add test/deintroverter/core_test.clj
git commit -m "test: verify EDN output and verbose mode"
```

---

## Plan Self-Review

| Spec requirement | Task |
|---|---|
| Babashka CLI | Task 1, 10 |
| File + recursive dir input | Task 2 |
| `.clj`/`.cljs`/`.cljc` | Task 2 |
| clojure.test + Speclj | Task 7, 8, 11 |
| SUT convention + require | Task 5 |
| deps.edn project boundary | Task 4 |
| Exclusion rules | Task 5 |
| Direct/let/`->`/`->>` tracing | Task 6 |
| Three verdicts | Task 6, 8 |
| Human + EDN output | Task 9 |
| `--verbose`, `--sut-ns`, `--exclude-ns`, `--project-root` | Task 10 |
| Exit 1 on introverted/questionable/errors | Task 9, 10 |
| Fixture-based tests | Tasks 8, 11, 12 |

No placeholders. All spec requirements mapped to tasks.