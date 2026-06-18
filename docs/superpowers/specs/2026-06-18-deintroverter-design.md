# deintroverter Design Spec

**Date:** 2026-06-18  
**Status:** Approved

## Purpose

`deintroverter` is a Babashka/Clojure CLI tool that scans test source files (`clojure.test` and Speclj) and classifies each test as extroverted, introverted, or questionable.

An **introverted test** is one whose assertions are not grounded in values produced by calling functions from the **system under test (SUT)**. The tool exists to surface tests that do not actually exercise the production code they appear to test.

## Problem Statement

Tests can give a false sense of coverage. A test may assert on setup data, mock return values, or local computations without ever calling a function from the system being tested. Such **introverted** tests pass but do not validate SUT behavior.

`deintroverter` performs static analysis on test files to flag introverted and uncertain tests, producing output suitable for humans and for agents that investigate deeper.

## Scope

### In scope (v1)

- Babashka-runnable CLI tool written in Clojure
- Input: one or more file or directory paths on the command line
- Recursive directory scanning for `.clj`, `.cljs`, `.cljc`
- Support for `clojure.test` (`deftest`, `testing`, `is`, `are`) and Speclj (`describe`, `context`, `it`, assertion macros)
- SUT namespace inference from test namespace convention and `:require`
- Project boundary detection via `deps.edn`
- Value tracing: direct SUT calls, `let` bindings, `->` and `->>` desugaring
- Three-way verdict per test: `:extroverted`, `:introverted`, `:questionable`
- Human-readable stdout and `--format edn` structured output
- Exit code 1 on introverted findings, questionable findings, or tool errors

### Out of scope (v1)

- Macro expansion or full semantic analysis (`tools.analyzer`)
- Tracing through destructuring, `as->`, `some->`, `cond->`, or custom macros
- Auto-fixing or rewriting tests
- `project.clj` / Leiningen support (may add later; v1 targets `deps.edn` projects)
- Runtime test execution

## Definitions

| Term | Meaning |
|---|---|
| **SUT** | System under test — in-project production namespaces whose functions should be called and whose results should be asserted |
| **Extroverted test** | At least one assertion provably traces to a call to a function in a SUT namespace |
| **Introverted test** | All assertions were analyzed and none trace to a SUT call |
| **Questionable test** | Analysis cannot reach a confident verdict (unresolved symbols, unsupported forms, unknown macros) |

### Verdict precedence (per test)

1. If any assertion is provably extroverted → test is `:extroverted`
2. Else if any assertion is `:questionable` → test is `:questionable`
3. Else (all assertions provably non-SUT) → test is `:introverted`

## Architecture

```
CLI (babashka)
  ├── path-resolver     → expand files/dirs (.clj .cljs .cljc, recursive)
  ├── project-context   → find deps.edn, build SUT namespace set
  ├── file-analyzer     → parse ns + test bodies per file
  └── reporter          → human stdout / EDN, exit codes
```

### Pipeline per file

1. Parse `ns` form → build alias→namespace map; collect `:require` candidates
2. Build SUT namespace set (see SUT Inference below)
3. Identify test boundaries (`deftest` or `it` forms)
4. For each test body: walk forms, accumulate `let` bindings, find assertions
5. For each assertion: trace asserted value to SUT calls
6. Apply verdict precedence to produce per-test result

### Analysis approach

Use **syntax-tree walk** with **edamame** for parsing. Do not macro-expand. Manually desugar `->` and `->>` in the value tracer.

Rejected alternatives:

- **Regex scanning** — too fragile for nested Clojure forms
- **tools.analyzer** — heavy JVM dependency, poor Babashka fit, overkill for v1 tracing depth

## SUT Inference

SUT namespaces are built from multiple sources, then filtered by exclusion rules.

### Sources (primary)

**A. Namespace convention**

Derive candidate SUT from the test namespace:

- `myapp.core-test` → `myapp.core`
- `myapp.core-test` (with `-test` suffix) → strip suffix and normalize

**C. `:require` in test `ns` form**

Namespaces required by the test file that pass exclusion rules and are in-project are SUT candidates.

### Overrides (special cases)

**B. Explicit CLI flags**

- `--sut-ns myapp.core` — add namespace(s) to SUT set
- `--exclude-ns myapp.helpers` — remove namespace(s) from SUT set

### Exclusion rules

A namespace is **not** SUT if it is:

1. In the standard Clojure suite (`clojure.*`)
2. A well-known third-party testing library (maintainable denylist):
   - `speclj.core` and Speclj namespaces
   - `clojure.test.check` / `test.check`
   - `bond.james`
   - Common mocking libraries (e.g. `shrubbery.core`, `atomist.test-support`)
   - Add entries as encountered; denylist is data, not hard-coded logic scattered through the codebase
3. From an **external dependency** declared in `deps.edn` (Maven coordinates, git libs, etc.)

### In-project namespace discovery

Read `deps.edn` from the project root:

- **Auto-discover:** walk up from each input file path until `deps.edn` is found
- **Override:** `--project-root /path` forces a specific root

From `deps.edn`:

- `:paths` entries identify local source directories
- Scan those directories for `ns` declarations to build the in-project namespace set
- Only in-project namespaces (minus exclusions) qualify as SUT

External deps (keys in `:deps` with coordinates) are excluded. Local deps (`:local/root`) are treated as in-project if under the project root.

## Per-Test Analysis

### Test identification

| Framework | Test boundary | Name |
|---|---|---|
| `clojure.test` | `(deftest name ...)` body | `name` symbol |
| Speclj | `(it "description" ...)` body | string description |

Nested Speclj `describe` / `context` blocks provide structure but do not affect the test boundary; only `it` forms are analyzed.

### Within each test body

1. Walk forms depth-first, accumulating a `let`-binding map: `{symbol → originating-form}`
2. On assertion forms, extract the **asserted value**:
   - `is` / `are`: the expression being checked (typically LHS of `=` or the predicate argument)
   - Speclj `should=`, `should==`: left-hand side; `should-be`: subject form; others per macro arity
3. Resolve function calls to namespaces using the `ns` alias map, `:refer`, and fully-qualified symbols
4. Trace the asserted value (see Tracing below)
5. Apply verdict precedence

### Assertion forms (v1)

| Framework | Forms |
|---|---|
| `clojure.test` | `is`, `are` |
| Speclj | `should=`, `should==`, `should-be`, `should-not`, `should-not-be`, `should-throw?`, `should-not-throw?` |

Unknown or custom assertion macros → assertion marked `:questionable` with reason `unknown-assertion-macro`.

## Value Tracing (v1)

| Mechanism | Supported | Notes |
|---|---|---|
| Direct SUT call in assertion | Yes | e.g. `(is (= 42 (calculate-total items)))` |
| Through `let` binding | Yes | Asserted symbol bound to form containing SUT call |
| Through `->` | Yes | Desugar: thread value as first arg of each step |
| Through `->>` | Yes | Desugar: thread value as last arg of each step |
| Destructuring in `let` | No | Mark `:questionable`, reason `destructuring` |
| `as->`, `some->`, `cond->` | No | Mark `:questionable`, reason `unsupported-threading-macro` |
| Java interop (`.method`) in thread | Partial | Handle `.method` forms in `->` steps; if unresolvable, `:questionable` |
| Anonymous fns `#()` in thread | No | Mark `:questionable`, reason `anonymous-fn` |
| Custom macros | No | Mark `:questionable`, reason `unknown-macro` |

### `->` desugaring

```
(-> x f g)        → (g (f x))
(-> x (f a) g)    → (g (f x a))
```

### `->>` desugaring

```
(->> x f g)       → (g (f x))
(->> x (f a) g)   → (g (f x a))   ; x inserted as last arg of (f a)
```

After desugaring, collect all function calls in the chain. If any resolved call targets a SUT namespace, the assertion is extroverted.

## CLI Interface

```
bb run deintroverter [options] <path> [<path> ...]
```

### Arguments

- One or more paths (files or directories)
- File → analyze that file
- Directory → recursively analyze all `.clj`, `.cljs`, `.cljc` files

### Options

| Flag | Purpose |
|---|---|
| `--format edn` | Emit structured EDN instead of human report |
| `--verbose` | Include `:extroverted` tests in output (hidden by default) |
| `--project-root PATH` | Override `deps.edn` auto-discovery |
| `--sut-ns NS` | Add SUT namespace (repeatable) |
| `--exclude-ns NS` | Exclude namespace from SUT set (repeatable) |

## Output

### Default (human-readable)

Report `:introverted` and `:questionable` tests only:

```
test/myapp/core_test.clj:42  (deftest calculate-total-test)  :introverted
  reason: no assertion traces to SUT call

test/myapp/core_test.clj:58  (it "handles edge case")  :questionable
  reason: asserted value via destructuring (unsupported)
```

With `--verbose`, also print `:extroverted` tests.

### EDN (`--format edn`)

```clojure
{:project-root "/path/to/project"
 :summary {:extroverted 12
           :introverted 3
           :questionable 2
           :errors 0}
 :findings [{:file "test/myapp/core_test.clj"
              :line 42
              :test-name "calculate-total-test"
              :test-form :deftest
              :verdict :introverted
              :reason :no-sut-assertion
              :sut-namespaces #{myapp.core}}
             {:file "test/myapp/core_test.clj"
              :line 58
              :test-name "handles edge case"
              :test-form :it
              :verdict :questionable
              :reason :destructuring
              :sut-namespaces #{myapp.core}}]
 :errors []}
```

### Error entries

Parse failures, unreadable files, and missing `deps.edn` (when required for project boundary) appear in `:errors`:

```clojure
{:type :parse-error :file "..." :line 10 :message "..."}
```

## Exit Codes

| Code | Condition |
|---|---|
| `0` | No introverted, no questionable, no errors |
| `1` | Any introverted finding, any questionable finding, or any tool error |

## Project Structure (proposed)

```
deintroverter/
  bb.edn
  src/deintroverter/
    core.clj          ; CLI entry
    paths.clj         ; path resolution
    project.clj       ; deps.edn / SUT namespace discovery
    parse.clj         ; edamame parsing helpers
    sut.clj           ; SUT inference and exclusion
    analyze.clj       ; per-file test analysis
    trace.clj         ; value tracing (->, ->>, let)
    assertions.clj    ; assertion form recognition
    report.clj        ; human + EDN output
  test/deintroverter/
    fixtures/         ; sample test files for each verdict
    *_test.clj         ; unit tests
  docs/superpowers/specs/
    2026-06-18-deintroverter-design.md
```

## Testing Strategy

Test the tool with `clojure.test`, runnable under Babashka. No dependency on external project suites.

### Fixture files

Small `.clj` snippets in `test/deintroverter/fixtures/` covering:

- `:extroverted` — direct SUT call, `let` binding, `->`, `->>`
- `:introverted` — assertions on literals, setup data, non-SUT calls only
- `:questionable` — destructuring, custom macros, unresolved symbols

### Unit test areas

- SUT inference: convention, `:require`, exclusions, `deps.edn` boundary
- Value tracing: direct, `let`, `->`, `->>`
- Framework parity: `deftest` and Speclj `it`
- CLI: exit codes, `--format edn`, `--verbose`, directory recursion
- Reporter: summary counts, finding structure

## Error Handling

| Situation | Behavior |
|---|---|
| Unreadable file | Add to `:errors`, continue other files, exit 1 |
| Parse error in file | Add to `:errors` with line number, skip file, exit 1 |
| No `deps.edn` found | Warn; fall back to namespace-convention-only SUT inference; do not treat as fatal |
| Empty input (no `.clj` files) | Exit 0 with empty summary |
| Unresolved symbol in assertion | Mark assertion `:questionable`, not an error |

## Future Enhancements

- `project.clj` / Leiningen project detection
- Tracing through `as->`, `some->`, destructuring
- Config file for denylist and SUT overrides (`.deintroverter.edn`)
- Speclj `before` / `with` block awareness
- JSON output format