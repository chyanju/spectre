# Changelog

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
