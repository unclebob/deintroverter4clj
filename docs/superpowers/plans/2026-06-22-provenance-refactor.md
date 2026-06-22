# Provenance Refactor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the growing collection of rescue heuristics in `analyze.clj` with a unified **provenance model** during the test-body walk, without regressing accuracy on fixtures or empire scans.

**Architecture:** Keep the two-tier verdict model, but collapse Tier 2 into provenance derivation + link checks. Tier 1 (`trace-form`) stays strict. Tier 2 becomes: “does any symbol in the asserted form resolve to SUT-linked provenance?” External heuristics (file I/O, stub wiring) remain explicit.

**Tech Stack:** Clojure, edamame, existing fixture suite (40 files), empire scan scripts

**Baseline (do not regress):**

| Metric | Current |
|--------|---------|
| Tests | 157 (339 assertions) |
| Empire introverted | 0 |
| `analyze.clj` | ~1500 LOC, ~10 `*-evidence` fns |

---

## Problem Statement

`deintroverter` today works as:

1. **Tier 1 — strict trace** (`trace.clj`): does the asserted form provably call/reference SUT?
2. **Tier 2 — rescue heuristics** (`analyze.clj`): walk saw SUT activity; promote anyway with a named evidence tag.

Each empire false-negative added another detector (`:sut-result-read`, `:exception-catch-assertion`, `catch-exception-marker`, `fixture-bindings-from-forms`, etc.). They share scaffolding but not a single data model.

**Target:** one provenance graph on bindings + walk-state; heuristics become derivation rules, not parallel `or` chains.

---

## Target Architecture

```
Test body walk
  ├── bindings: sym → {:value <form> :prov <Provenance>}
  └── walk-state: {:seen-sut? :last-sut-prov :world-mutations :stub-captures}

Assertion
  ├── strict trace (trace-form) → :extroverted
  ├── provenance link → :likely-extroverted
  └── external heuristic (files, stubs) → :likely-extroverted + subtype

Provenance kinds
  :sut-invoke | :sut-derived | :catch-derived | :fixture
  | :stub-capture | :world-mutation | :test-module | :literal | :unknown
```

**Confidence mapping (unchanged intent):**

| Link | Verdict |
|------|---------|
| Strict trace proves SUT | `:extroverted` |
| Provenance link (indirect) | `:likely-extroverted` |
| External world (file, stub wiring) | `:likely-extroverted` + subtype |
| No link | `:introverted` |

---

## File Map (post-refactor)

| File | Responsibility |
|------|----------------|
| `src/deintroverter/provenance.clj` | **NEW** — types, `derive-provenance`, `merge-provenance`, `provenance-link?` |
| `src/deintroverter/walk.clj` | **NEW** — stack walk, step dispatch, helper inlining |
| `src/deintroverter/heuristics/external.clj` | **NEW** — file-dependency, stub-wiring only |
| `src/deintroverter/trace.clj` | Strict SUT reachability; absorb analyze-only reach patches |
| `src/deintroverter/analyze.clj` | Orchestration only (~300 LOC target) |
| `test/deintroverter/provenance_test.clj` | **NEW** — pure derivation tests |
| `test/deintroverter/golden_findings.edn` | **NEW** — snapshot contract |
| `test/deintroverter/golden_test.clj` | **NEW** — golden diff test |
| `scripts/check_empire_introverted.clj` | **NEW** — CI gate (introverted = 0) |

---

## PR Dependency Graph

```
PR0  golden snapshot + empire gate
  └─ PR1  provenance core + unit tests
       └─ PR2  wire provenance into walk (bridge to legacy evidence)
            └─ PR3a migrate sut-result + exception-catch
            └─ PR3b migrate direct-assertion + nested invoke
            └─ PR3c collapse side-effect-evidence; delete sentinels
                 └─ PR4a trace consolidation
                 └─ PR4b walk.clj extraction
                      └─ PR5  reason taxonomy
```

---

## Task 0: Baseline & contracts (PR0)

**Files:**
- Create: `test/deintroverter/golden_findings.edn`
- Create: `test/deintroverter/golden_test.clj`
- Create: `scripts/check_empire_introverted.clj`
- Create: `test/deintroverter/fixtures_manifest.edn` (negative fixture index)

- [ ] **Step 1: Add fixture manifest**

Tag fixtures that must stay introverted/questionable:

```edn
{:negative-introverted
 ["introverted_literal.clj"
  "empire_stamping_negative.clj"]
 :negative-paired-second-examples
 ["nested_sut_assertion.clj"
  "exception_catch_assertion.clj"
  "speclj_with_fixture.clj"
  "helper_destructure_result.clj"
  "debug_sut_result.clj"
  "side_effect_helpers.clj"
  "stub_capture_wiring.clj"
  "world_atom_readback.clj"
  "file_dependency.clj"]}
```

- [ ] **Step 2: Generate golden findings snapshot**

Script or test helper that runs `analyze/analyze-file` on every file in `test/deintroverter/fixtures/*.clj` (excluding `sample-project/`) and writes EDN:

```clojure
[{:fixture "extroverted_direct.clj"
  :test-name "..."
  :verdict :extroverted
  :reason nil
  :evidence nil} ...]
```

Include `:side-effect-evidence`, `:direct-assertion-evidence`, `:wiring-evidence` when present.

- [ ] **Step 3: Write `golden_test.clj`**

`deftest golden-findings-unchanged` — compare live output to `golden_findings.edn`. Fail on any diff with readable report.

- [ ] **Step 4: Add empire gate script**

`scripts/check_empire_introverted.clj` — wrap `categorize_introverted.clj`, exit 1 if count > 0. Document `PROJECT_ROOT` env var.

- [ ] **Step 5: Verify baseline**

```bash
clojure -M:cov
PROJECT_ROOT=/path/to/empire/empire-2025 bb scripts/check_empire_introverted.clj
```

Expected: 157 tests pass; empire introverted 0.

- [ ] **Step 6: Commit**

```bash
git add test/deintroverter/golden_* test/deintroverter/fixtures_manifest.edn scripts/check_empire_introverted.clj
git commit -m "test: golden findings snapshot and empire introverted gate"
```

---

## Task 1: Provenance core (PR1)

**Files:**
- Create: `src/deintroverter/provenance.clj`
- Create: `test/deintroverter/provenance_test.clj`

- [ ] **Step 1: Define provenance record**

```clojure
{:kind :sut-invoke | :sut-derived | :catch-derived | :fixture
         | :stub-capture | :world-mutation | :test-module
         | :literal | :unknown
 :source <form>
 :confidence :proven | :likely
 :via [:destructure :get :ex-data :deref :helper-inline ...]}
```

Public fns: `literal-provenance`, `sut-invoke-provenance`, `catch-derived-provenance`, `derive-provenance`, `merge-provenance`, `provenance-kind`, `provenance-confidence`.

- [ ] **Step 2: Implement derivation rules**

| Expression | Provenance |
|------------|------------|
| SUT call (`trace/reaches-sut?`) | `:sut-invoke` |
| `(get x :k)` where `x` is SUT-linked | `:sut-derived` |
| Destructure from SUT value | `:sut-derived` |
| `(ex-data e)` where `e` is `:catch-derived` | `:catch-derived` |
| `(deref sym)` | propagate `sym`'s provenance |
| Literal | `:literal` |
| Unknown / unresolved | `:unknown` |

Cap derivation depth at 3 hops; store `:via` chain.

- [ ] **Step 3: Implement `provenance-link?`**

```clojure
(provenance-link? asserted-form binding-env walk-state trace-ctx)
;; => {:linked? true :kind :sut-derived :source <form>} | nil
```

Kinds that link: `:sut-invoke`, `:sut-derived`, `:catch-derived` (with `(:seen-sut? walk-state)` guard for catch).

Kinds that do **not** auto-link: `:literal`, `:unknown`, `:test-module`.

- [ ] **Step 4: Write `provenance_test.clj` (≥20 cases)**

Cover:
- `(get (run-helper) :result)` → `:sut-derived`
- `{a :result}` destructure → `:sut-derived`
- `(ex-data e)` after catch → `:catch-derived`
- `(deref initial-map)` with fixture prov → propagates
- literal `"plain"` → no link
- derivation depth cap

- [ ] **Step 5: Run tests**

```bash
clojure -M:cov
```

Expected: all pass; golden unchanged.

- [ ] **Step 6: Commit**

```bash
git commit -m "feat: provenance core with derivation and link checks"
```

---

## Task 2: Wire provenance into walk (PR2)

**Files:**
- Modify: `src/deintroverter/analyze.clj`
- Create: `test/deintroverter/walk_provenance_test.clj` (optional, or extend `provenance_test.clj`)

- [ ] **Step 1: Extend binding env (parallel path)**

Introduce `binding-prov` alongside existing flat `bindings` map. Wrappers:

```clojure
(defn- assoc-binding [env sym value prov] ...)
(defn- lookup-prov [env sym] ...)
(defn- syms-in-env [env] ...)
```

Do not remove flat bindings yet — both paths live until PR3.

- [ ] **Step 2: Extend walk-state**

Add `:last-sut-prov` alongside `:last-sut-call`. Set in `advance-walk-state` when SUT reached.

- [ ] **Step 3: Emit provenance in step handlers**

| Handler | Change |
|---------|--------|
| `process-let-step` | `derive-provenance` per binding |
| `process-catch-step` | `e` → `catch-derived-provenance` (keep sentinel until PR3c) |
| `fixture-bindings-from-forms` | `derive-provenance` on `with` expr |
| `process-fn-invoke-step` | propagate arg prov via `invoke-arg-value` |

- [ ] **Step 4: Bridge to legacy evidence tags**

```clojure
(defn- provenance-link-evidence [asserted-form env walk-state trace-ctx]
  (when-let [link (provenance/link? ...)]
    (case (:kind link)
      :sut-derived :sut-result-read
      :catch-derived :exception-catch-assertion
      :sut-invoke :sut-result-read
      nil)))
```

Call from `side-effect-evidence` **before** legacy detectors. If bridge returns a tag, use it; else fall through to legacy.

- [ ] **Step 5: Verify golden identical**

```bash
clojure -M:cov
# golden-findings-unchanged must pass with zero diff
PROJECT_ROOT=.../empire-2025 bb scripts/check_empire_introverted.clj
```

- [ ] **Step 6: Commit**

```bash
git commit -m "feat: wire provenance into walk with legacy evidence bridge"
```

---

## Task 3a: Migrate sut-result + exception-catch (PR3a)

**Files:**
- Modify: `src/deintroverter/analyze.clj`
- Modify: `src/deintroverter/provenance.clj`

- [ ] **Step 1: Route `sut-result-read-evidence` through provenance**

Delete `let-bound-sut-result-syms`, `sut-result-rhs-form`, `sut-result-binding-value` once bridge covers all fixture cases.

- [ ] **Step 2: Route `exception-catch-assertion-evidence` through provenance**

Delete `catch-exception-marker`, `catch-exception-sym?`, `ex-data-from-catch-exception?`, `catch-derived-binding-syms` once bridge covers:
- `exception_catch_assertion.clj`
- `pipeline_var_deref.clj`
- empire `ui/util/core_spec.clj` (via empire gate)

- [ ] **Step 3: Run full regression**

```bash
clojure -M:cov
bb scripts/check_empire_introverted.clj  # with PROJECT_ROOT set
```

- [ ] **Step 4: Commit**

```bash
git commit -m "refactor: migrate sut-result and exception-catch to provenance"
```

---

## Task 3b: Migrate direct-assertion + nested invoke (PR3b)

**Files:**
- Modify: `src/deintroverter/analyze.clj`
- Modify: `src/deintroverter/provenance.clj`

- [ ] **Step 1: Express `asserted-production-invoke?` as provenance + strict trace**

Asserted subject tree: any subform with `:sut-invoke` provenance → link. Keep `:nested-sut-invoke` / `:sut-invoke` evidence tags in trace output.

- [ ] **Step 2: Fold `resolve-asserted-subject` into provenance lookup**

Symbol subjects resolve through binding env prov before tree walk.

- [ ] **Step 3: Verify fixtures**

- `nested_sut_assertion.clj` — first test extroverted
- `assertion_sut_invoke.clj` — unchanged
- `helper_destructure_result.clj` — likely-extroverted

- [ ] **Step 4: Commit**

```bash
git commit -m "refactor: migrate direct-assertion path to provenance"
```

---

## Task 3c: Collapse side-effect-evidence (PR3c)

**Files:**
- Modify: `src/deintroverter/analyze.clj`
- Create: `src/deintroverter/heuristics/external.clj`

- [ ] **Step 1: Replace `side-effect-evidence` or-chain**

```clojure
(or (provenance-link-evidence ...)
    (external/world-atom-readback-evidence ...)
    (external/immediate-preceding-sut-evidence ...)
    (external/test-state-binding-evidence ...))
```

Move file-dependency and stub-wiring to `heuristics/external.clj` — **do not** fold into generic provenance.

- [ ] **Step 2: Simplify `process-parsed-assertion`**

```clojure
(cond
  stub-invocation? ...
  parsed-reason? ...
  (external/evidence? ...) ...
  (strict-extroverted? ...) ...
  (provenance-link? ...) ...
  :else introverted)
```

- [ ] **Step 3: Delete dead code**

Remove legacy fns only when golden + empire pass. Target: ≤3 `*-evidence` fns remaining (all in `external.clj`).

- [ ] **Step 4: Line count check**

`analyze.clj` should drop ≥150 LOC from peak.

- [ ] **Step 5: Commit**

```bash
git commit -m "refactor: collapse side-effect evidence into provenance + external"
```

---

## Task 4a: Trace consolidation (PR4a)

**Files:**
- Modify: `src/deintroverter/trace.clj`
- Modify: `src/deintroverter/provenance.clj`
- Modify: `test/deintroverter/trace_test.clj`

- [ ] **Step 1: Add unified reach API**

```clojure
(trace/form-sut-level form bindings ctx)
;; => :proven | :likely | :none
```

Absorb from `analyze.clj`:
- `var-deref-reaches-sut?`
- `helper-invoke-reaches-sut?`
- `bound-invoke-reaches-sut?`
- `namespaced-production-invoke?`

- [ ] **Step 2: Update `derive-provenance` to call `form-sut-level`**

Remove duplicate reach logic from `provenance.clj`.

- [ ] **Step 3: Extend `trace_test.clj`**

Cases for `@#'var`, helper invoke, bound invoke.

- [ ] **Step 4: Commit**

```bash
git commit -m "refactor: consolidate SUT reachability in trace.clj"
```

---

## Task 4b: Extract walk.clj (PR4b)

**Files:**
- Create: `src/deintroverter/walk.clj`
- Modify: `src/deintroverter/analyze.clj`

- [ ] **Step 1: Move walk machinery**

Extract to `walk.clj`:
- `process-one-form`, stack frames, `head-form-steps`
- `process-fn-invoke-step`, `invoke-arg-value`
- `advance-walk-state`, `fresh-walk-state`
- All `process-*-step` fns

- [ ] **Step 2: Public walk API**

```clojure
(walk/process-forms forms bindings trace-ctx opts)
;; => {:results [...] :walk-state ...}
```

- [ ] **Step 3: Slim `analyze.clj`**

Target ≤400 LOC: `find-tests` + `findings-for-forms` + verdict aggregation only.

- [ ] **Step 4: Coverage check**

`provenance.clj` > 95% forms; no coverage regression on `analyze.clj` + `walk.clj` combined.

- [ ] **Step 5: Commit**

```bash
git commit -m "refactor: extract walk.clj from analyze.clj"
```

---

## Task 5: Reason taxonomy (PR5)

**Files:**
- Modify: `src/deintroverter/analyze.clj`
- Modify: `src/deintroverter/report.clj`
- Modify: `test/deintroverter/analyze_test.clj`
- Modify: `test/deintroverter/golden_findings.edn`

- [ ] **Step 1: Normalize reason keywords**

| Legacy | New |
|--------|-----|
| `:sut-side-effect-heuristic` | `:provenance-link` |
| `:sut-direct-assertion-heuristic` | `:provenance-link` |
| `:sut-wiring-heuristic` | `:external-wiring` |
| `:file-dependency` | `:external-file` |

Add trace detail: `:link-via` (`:sut-derived`, `:catch-derived`, `:sut-invoke`), `:prov-source`.

- [ ] **Step 2: Backward-compat alias (one release)**

EDN output includes `:reason-legacy` mapping to old keywords for downstream consumers.

- [ ] **Step 3: Update golden + analyze tests**

Regenerate `golden_findings.edn` with new reasons. Update assertions to check `:reason` and `:link-via`.

- [ ] **Step 4: Commit**

```bash
git commit -m "feat: normalized provenance reason taxonomy with legacy aliases"
```

---

## Regression Gates (every PR)

```bash
# Unit + integration
clojure -M:cov

# Golden contract (after PR0)
clojure -M -e "(require '[clojure.test :refer [run-tests]]) (run-tests 'deintroverter.golden-test)"

# Empire scan
PROJECT_ROOT=/path/to/empire/empire-2025 bb scripts/check_empire_introverted.clj
```

**Must hold:**

- 157 tests, 0 failures
- Golden diff empty (or intentionally regenerated with review)
- Empire introverted = 0
- Negative fixtures from `fixtures_manifest.edn` unchanged

---

## Risk Register

| Risk | Mitigation |
|------|------------|
| Over-broad `:sut-derived` | Depth cap 3; require `:via` chain in trace |
| False extroverted via helper inline | Only inline `defn-` in test ns; body must reach SUT for promotion |
| Catch-derived without preceding SUT | Guard `(and (:seen-sut? ws) (provenance-link? ...))` |
| Silent regression | Golden EDN + empire gate in every PR |
| Big-bang refactor | Phases 1–2 ship without deleting legacy; one heuristic per PR in Phase 3 |
| Lost auditability | Keep `:link-via` and `:prov-source` in trace EDN |

---

## Success Metrics

| Metric | Target |
|--------|--------|
| `analyze.clj` LOC | < 400 |
| Named `*-evidence` fns outside `external.clj` | 0 |
| Empire introverted | 0 |
| Golden diff | empty |
| False extroverted on negative fixtures | 0 |
| `provenance.clj` coverage | > 95% |

---

## Out of Scope

- Macro expansion / `tools.analyzer`
- ML or fuzzy matching
- Merging file/stub heuristics into generic provenance taint
- Changing verdict precedence (extroverted > questionable > introverted)

---

## Optional Follow-ups (post-plan)

- [ ] Framework plugin interface (`speclj/with`, `clojure.test` fixtures as registrable provenance sources)
- [ ] `--verbose` provenance chain printer for agent investigation
- [ ] Mutation tests: remove guards on negative fixtures and assert promotion fails

---

## Progress Tracker

| PR | Task | Status |
|----|------|--------|
| PR0 | Task 0: Baseline & contracts | - [ ] |
| PR1 | Task 1: Provenance core | - [ ] |
| PR2 | Task 2: Wire into walk | - [ ] |
| PR3a | Task 3a: sut-result + exception-catch | - [ ] |
| PR3b | Task 3b: direct-assertion | - [ ] |
| PR3c | Task 3c: collapse side-effect-evidence | - [ ] |
| PR4a | Task 4a: trace consolidation | - [ ] |
| PR4b | Task 4b: walk.clj extraction | - [ ] |
| PR5 | Task 5: reason taxonomy | - [ ] |