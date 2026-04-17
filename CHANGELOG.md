# Changelog

## v0.1.5.1 — 2026-04-16

### Fixed

- **SPEC-LIFE-004**: Fixed logic error in the "consuming choice creates different template type" guard. Previously, any choice that also re-created the original template via `create this with {...}` would suppress ALL transmutation findings, even when the choice also created a foreign template type. The guard now correctly only suppresses when a same-type replacement exists.
- **SPEC-INV-011**: Fixed local `isTimeField` definition that shadowed the richer `Rules.Utils.isTimeField`. The local version only recognized `Time`, missing `RelTime` and `DA.Time.Time`. The rule now uses the shared utility for consistent type detection.
- **CLI**: Version banner in `--verbose` mode now uses `Paths_spectre` for the version string instead of a hardcoded value that was stale at `v0.1.3.0`.
- **CLI**: Fixed verbose mode showing `[parsed]` status for files that failed parsing, and parse errors being printed twice in verbose mode.
- **Parser**: Removed duplicate `"where"` entry in `damlKeywords`.
- **Benchmark.hs**: Replaced `mapMaybe id` with `catMaybes`.

### Removed

- Unused `vector` package dependency.
- Dead `"!="` operator from `extractMentionedVars` in SPEC-INV-014 (`!=` is not a valid DAML operator; DAML uses `/=`).

## v0.1.5.0 — 2026-04-16

### Changed

- **SPEC-VIS-001 / SPEC-VIS-002**: Reduced false positives by filtering out synthetic `_Snippet` templates (parser artifact with empty signatory/observer lists). Added `isChoiceParamWithStakeholderCoController` to suppress findings where a choice-parameter controller has at least one stakeholder co-controller — a legitimate multi-controller pattern in Canton where the non-stakeholder party gains visibility via the exercise mechanism.
- **SPEC-LIFE-004**: `findCreatedTemplateNames` now accepts the enclosing template name and correctly recognizes `create this with {...}` (parsed as `SCreate (TCon "this")`, `ERecordUpd (EVar "this")`, or `EVar "this"`) as self-recreation rather than transmutation to a "different type."
- **SPEC-DIAG-004**: `collectFromExpr` for `EFieldAccess` now adds the field name to the reference set (previously only the sub-expression was collected). Data-type-derived record accessor names are excluded from the "unused definition" check since they are auto-generated.
- **SPEC-INV-014**: Generalized `extractZeroGuards` into `extractDenominatorGuards`. Previously used a brittle 24-case pattern match only recognizing literal `0`/`0.0` comparisons. Now recognizes any assertion whose condition mentions the denominator variable in a comparison operator, including named-constant guards (`v > minDenom`) and helper-function guards (`assertPositive v`).
- **SPEC-INV-008**: Removed overly generic `"value"` from `nameHintIsNumeric` fallback list (would false-positive on any field containing "value"). `findFieldsWithOnlyLowerBoundInEnsure` now uses type-based `isNumericField` from `Rules.Utils` instead of the name-heuristic `isNumericFieldName`, since template fields have type information available.
- **SPEC-DIAG-005**: Removed `"Get"` from `validationPrefixes` — `Get*` is a common legitimate consuming choice naming pattern (e.g., `GetAndUpdate`).

### Benchmark

- 40 rules, 45 cases evaluated
- Precision 94%, Recall 77%, F1 85%
- 34 TP, 21 TN, 2 FP, 10 FN
- Recall decrease from v0.1.4.0 (84% → 77%) is due to removal of coincidental true-positive matches where LIFE-004 and VIS-001 were providing TPs via incorrect detections (e.g., flagging `create this with` as "different type"). These were false-positive-quality findings counted as TPs by the benchmark system.

## v0.1.4.0 — 2026-04-16

### Added

- **SPEC-INV-014**: New rule to detect unsafe division (`/`) by a variable denominator without prior zero-check. Can prevent critical division-by-zero crashes (DoS) in DAML `Decimal` calculations.
- **SPEC-SCALE-004**: New rule to detect observer set bloat. Identifies templates using `[Party]` list fields as observers and appending to them via `::` or `++` inside choices.
- **Benchmarks**: Added two new synthetic benchmark cases `DOC-INV-02` (Unsafe division) and `DOC-SCALE-04` (Observer bloat) to evaluate the new rules. Synchronized the benchmark schema and corpus.

### Fixed

- **Parser**: Added native support for the DAML cons/list-append operator (`::`) as an infix operator in `EInfix`, ensuring proper AST generation for list growth operations.
- **Rule handlers**: Improved the robustness of zero-guards in `SPEC-INV-014` to recognize not only `/=` and `!=`, but also `<`, `>`, `<=`, and `>=` against `0.0` or `0`, drastically reducing false positives in real-world financial contracts.

## v0.1.3.0 — 2026-04-15

### Fixed

- **Parser**: Lowercase-initial identifiers (e.g., `transfer.requestedAt`) no longer greedily consume dots. They are now correctly parsed as `EFieldAccess` instead of a single `EVar` with a dotted name. Qualified module references (uppercase-initial, e.g., `DA.Set.fromList`) are unaffected.
- **Rule handlers**: Updated `collectDottedRefs`, `fetchVarsReferenced`, `exprRefsName` (Invariant.hs) and `exprMentions` (Authorization.hs) to handle the new correct `EFieldAccess` representation.

### Removed

- **SPEC-TEMP-004** (Settlement execution without deadline assertion): Removed for violating the generality principle — the rule hardcoded settlement-specific function names rather than detecting a general pattern. Rule count: 39 → 38, Temporal: 4 → 3.

### Changed

- **SPEC-INV-013**: Rewritten with a structural detection strategy. Previously hardcoded specific function names; now uses two general approaches: (A) type-based detection for choice parameters with explicit `[T]` list annotations, and (B) field-access pattern matching for standalone functions. No name heuristics.
- **SPEC-TEMP-001**: Removed benchmark-specific keywords (`"allocatebefore"`, `"transferbefore"`) that were already covered by the general `"before"` pattern.
- **README**: Fixed `--enable` → `--rule` CLI flag documentation.

### Benchmark

- 38 rules, 30 cases evaluated, 30 parsed
- Precision 100%, Recall 97%, F1 98%
- 28 TP, 17 TN, 0 FP, 1 FN

## v0.1.2.0 — 2026-04-15

### Changed

- Renamed CLI subcommand `lint` to `analyze` to better reflect the tool's role as a security analyzer rather than a style linter
- Added project logo and Quick Start section to README

## v0.1.0.0 — 2026-04-15

Initial release.

### Features

- Full DAML parser built on Megaparsec with support for templates, choices, interfaces, data types, and expressions
- 39 security inspection rules across 10 modules:
  - Authorization (2): signatory model, role-based access control
  - Visibility (2): observer/controller visibility gaps
  - Temporal (4): deadline enforcement, time handling, phase separation
  - Invariant (13): input validation, partial functions, data consistency
  - State (2): collection operations, state transition symmetry
  - Lifecycle (4): contract creation/archival patterns
  - Scalability (3): resource bounds, contract bloat
  - CrossTemplate/Diagnostics (6): error messages, naming, audit trails
  - Upgrade (2): version compatibility, interface views
  - Integration (1): cross-system parameter validation
- CLI with three subcommands: `analyze`, `benchmark`, `rules`
- Human-readable and JSON output formats
- Configurable rule enable/disable via `--rule` and `--disable` flags
- 38-case labeled benchmark dataset with differential evaluation
- Benchmark results: 100% precision, 97% recall, 98% F1 score

### Benchmark Dataset Sources

- Temple DAML Contracts (Halborn audit) — 8 cases
- Temple Diff Review (Halborn) — 4 cases
- Obsidian Tradecraft AMM (Halborn audit) — 12 cases
- Pattern-derived synthetic pairs — 9 cases
- GitHub-derived (Canton + DAML releases) — 5 cases
