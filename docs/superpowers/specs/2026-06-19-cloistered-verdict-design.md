# Cloistered Verdict Design

## Summary

Split `:introverted` into `:introverted` (no SUT, no test-module reach) and
`:cloistered` (no SUT, but reaches other test modules). Remove verdict-based
exit codes; the report is the sole indicator. Parse errors appear in `:errors`
only.

## Verdicts

| Verdict | SUT | Test modules |
|---------|-----|--------------|
| `:extroverted` / `:likely-extroverted` | Yes | — |
| `:questionable` | Uncertain | — |
| `:cloistered` | No | Yes |
| `:introverted` | No | No |

Precedence through `:questionable` is unchanged. Only former `:introverted`
tests are split using test-module reach across the whole test body.

## Test module (option B)

A namespace qualifies when **either**:

- its name ends with `-spec` or `-test`, or
- its source file path contains `test/` or `spec/` under the project root

Excluded: current test namespace, SUT namespaces, `clojure.*`, denylisted
framework libs, external dependency keys.

## Exit code

Always `0`. Findings and errors are reported in human and EDN output.

## Implementation

- `project/load-context` adds `:namespace-paths` (ns → relative path)
- `test-modules` identifies test-module namespace set per file
- `trace/reaches-test-module?` walks test body forms
- `analyze` promotes introverted → cloistered when body reaches test modules
- `report` adds `:cloistered` summary bucket; `exit-code` returns `0`