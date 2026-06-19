# deintroverter

Static analyzer for Clojure and Speclj tests. Classifies each test as **extroverted**, **likely-extroverted**, **introverted**, or **questionable** based on whether assertions trace to the system under test (SUT).

An introverted test passes but does not ground its assertions in SUT behavior — for example, asserting on literals, test-local data, or `clojure.core` helpers without calling production code.

## Requirements

- [Babashka](https://babashka.org/)

## Usage

```bash
bb -m deintroverter.core [options] <paths...>
```

Or via the `bb.edn` task:

```bash
bb run deintroverter [options] <paths...>
```

### Examples

```bash
# Scan a project's spec directory
bb -m deintroverter.core --project-root ../my-app spec/

# Machine-readable report for agents
bb -m deintroverter.core --format edn --project-root ../my-app spec/

# Show extroverted tests too
bb -m deintroverter.core --verbose spec/
```

### Stack depth on large projects

Deeply nested test bodies (common in large Speclj suites) can exhaust Babashka's default JVM stack and fail with `StackOverflowError`. Increase the stack size with `BB_JVM_OPTS`:

```bash
BB_JVM_OPTS="-Xss32m" bb -m deintroverter.core --project-root ../my-app spec/
```

Start with `32m`; if errors persist, try `64m`.

**Full-directory scans.** Even with a larger stack, pointing deintroverter at a large `spec/` tree in one invocation can still overflow — analysis is recursive and stack use accumulates across files in a single process. If a whole-directory scan fails, scan subdirectories or individual files instead:

```bash
BB_JVM_OPTS="-Xss32m" bb -m deintroverter.core --project-root ../my-app spec/my_app/game_logic/
BB_JVM_OPTS="-Xss32m" bb -m deintroverter.core --project-root ../my-app spec/my_app/core_spec.clj
```

Each run is independent; combine the results manually or with a small wrapper script.

### Options

| Option | Description |
|--------|-------------|
| `-h`, `--help` | Print usage and exit |
| `--format edn` | Print structured EDN instead of human-readable lines |
| `--verbose` | Include extroverted and likely-extroverted tests in human output |
| `--project-root <path>` | Project root for `deps.edn` discovery and in-project namespace boundaries |
| `--sut-ns <namespace>` | Add a namespace to the SUT set |
| `--exclude-ns <namespace>` | Remove a namespace from the SUT set |

Paths may be files or directories. Directories are scanned recursively for `.clj`, `.cljs`, and `.cljc` files.

## Verdicts

| Verdict | Meaning |
|---------|---------|
| `:extroverted` | At least one assertion traces to the SUT (function call, var read, private var invoke, or value derived from a SUT-bound `let`) |
| `:likely-extroverted` | Unqualified call via `:refer :all` from a SUT namespace (heuristic; not proven) |
| `:introverted` | All assertions analyzed; none trace to the SUT |
| `:questionable` | Analysis could not reach a confident verdict |

Likely-extroverted findings are hidden in default human output and do **not** affect the exit code.

### Exit code

- `0` — no introverted, questionable, or parse errors
- `1` — at least one introverted or questionable test, or a parse error

## Output

### Human (default)

```
spec/my_app/core_spec.clj:42  (it adds totals)  :introverted
  reason: no-sut-assertion
```

By default, only introverted and questionable tests are printed. Use `--verbose` to include extroverted and likely-extroverted tests.

### EDN (`--format edn`)

```clojure
{:project-root "..."
 :summary {:extroverted 42
           :likely-extroverted 0
           :introverted {:total 3 :no-sut-assertion 3}
           :questionable {:total 17
                           :unknown-assertion-macro 14
                           :destructuring 3}
           :errors 0}
 :findings [{:file "..." :line 42 :test-name "..."
             :test-form :it :verdict :introverted
             :reason :no-sut-assertion
             :sut-namespaces #{my-app.core}
             :trace {:test-ns my-app.core-test
                      :requires #{my-app.core}
                      :refer-syms {calculate-total my-app.core}
                      :sut-namespaces #{my-app.core}
                      :assertions [{:asserted-form (calculate-total items)
                                    :calls-traced [{:sym calculate-total
                                                    :resolved-ns my-app.core
                                                    :level :proven}]
                                    :binding-origins []
                                    :verdict :extroverted
                                    :reason nil}]}}]
 :errors []}
```

The summary breaks introverted and questionable counts down by `:reason` so agents can triage without scanning every finding.

## SUT inference

Namespaces are treated as SUT when they are:

1. Inferred from the test namespace (`foo.bar-test` or `foo.bar-spec` → `foo.bar`)
2. Listed in the test file's `:require` clause
3. In the project (per `deps.edn` paths) and not excluded

Excluded by default: `clojure.*`, common test libraries (Speclj, `clojure.test`, etc.), and external dependency namespaces.

## What gets traced

The analyzer walks test bodies and traces asserted expressions to:

- Direct SUT function calls (including aliased and `:refer` names)
- Private function calls via `#'ns/fn` / `(var ns/fn)`
- Reads of SUT vars (e.g. `m/rules`)
- `let` bindings — including projections like `(:action command)` when `command` came from a SUT call
- Threading macros `->` and `->>` (desugared)

Transparent wrappers (`do`, `try`, `let`, `with-redefs`, `doseq`) are walked through to inner assertions.

Unsupported forms are marked questionable: destructuring in `let`, anonymous fns in assertions, `as->` / `some->` / `cond->` in asserted values, and unrecognized assertion macros.

### Supported assertion macros

**clojure.test:** `is`, `are`

**Speclj:** `should`, `should=`, `should==`, `should-not`, `should-be`, `should-not-be`, `should-be-nil`, `should-not-be-nil`, `should-contain`, `should-be-a`, `should-throw`, `should-not-throw`, `should-throw?`, `should-not-throw?`

## Development

```bash
bb test
```

Design spec and implementation plan: `docs/superpowers/specs/2026-06-18-deintroverter-design.md`