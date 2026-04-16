# Benchmark Dataset

Spectre includes a labeled dataset of 38 DAML smart contract security cases for evaluating analysis tool accuracy. Each case contains a buggy (vulnerable) version, a fixed version, structured metadata, and a diff patch.

## Running the Benchmark

```bash
stack exec spectre -- benchmark --dir benchmarks -v
```

This parses all buggy and fixed files, runs the relevant rules for each case's pattern family, classifies results as TP/TN/FP/FN, and computes aggregate precision/recall/F1 metrics.

## Dataset Structure

```
benchmarks/
├── corpus.json          # Master index of all cases (inline metadata)
├── schema.json          # JSON Schema for meta.json files
├── README.md            # Dataset overview
│
├── audit-derived/       # Real findings from professional security audits
│   ├── temple/          # Temple DAML Contracts - Halborn audit (8 cases)
│   │   └── TEMPLE-HAL-01/ ... TEMPLE-HAL-08/
│   ├── temple-diff/     # Temple Diff Review - Halborn (4 cases)
│   │   └── TEMPLE-DIFF-01/ ... TEMPLE-DIFF-04/
│   └── obsidian/        # Obsidian Tradecraft AMM - Halborn (12 cases)
│       └── OBS-HAL-01/ ... OBS-HAL-12/
│
├── pattern-derived/     # Synthetic positive/negative code pairs (9 cases)
│   ├── DOC-AUTH-01/     # Missing role authorization check
│   ├── DOC-DIAG-01/     # Deletable audit record
│   ├── DOC-INTEG-01/    # Missing expected-admin validation
│   ├── DOC-INV-01/      # Asymmetric validation
│   ├── DOC-LIFE-01/     # Orphaned contract reference
│   ├── DOC-SCALE-01/    # Unbounded observer set
│   ├── DOC-TEMP-01/     # Missing deadline enforcement
│   ├── DOC-UPGRADE-01/  # Interface view field drop
│   └── DOC-VIS-01/      # Controller not listed as observer
│
└── github-derived/      # Cases from GitHub release notes (5 cases)
    ├── canton-releases/
    │   └── CANTON-REL-01/ ... CANTON-REL-04/
    └── daml-releases/
        └── DAML-REL-01/
```

## Case Directory Layout

Each case follows a consistent structure:

```
{CASE-ID}/
├── meta.json      # Structured metadata (conforms to schema.json)
├── buggy/         # Pre-fix (vulnerable) DAML source files
│   └── *.daml
├── fixed/         # Post-fix (corrected) DAML source files
│   └── *.daml
└── diff.patch     # Unified diff between buggy and fixed
```

## Metadata Schema (meta.json)

Key fields in each case's `meta.json`:

| Field | Type | Description |
|-------|------|-------------|
| `id` | string | Unique case identifier (e.g., `TEMPLE-HAL-01`) |
| `title` | string | Short descriptive title |
| `artifact_type` | enum | `finding`, `architectural_observation`, `release_note_bugfix`, `pattern_example` |
| `source_kind` | enum | `audit_report`, `feedback_review`, `release_notes`, `documentation` |
| `pattern_family` | enum | One of 9 pattern families (see below) |
| `severity` | enum | `critical`, `high`, `medium`, `low`, `informational` |
| `evidence_strength` | enum | `strong`, `moderate`, `weak`, `synthetic` |
| `bvss_score` | number | BVSS score from audit report (when available) |
| `oracle` | object | Detection hints: `type`, `description`, `static_signal` |
| `good_for_static` | boolean | Whether the case is suitable for static analysis evaluation |
| `good_for_symbolic` | boolean | Whether the case is suitable for symbolic execution evaluation |
| `good_for_benchmark` | boolean | Whether to include in the primary benchmark set |
| `code_snippets` | object | Paths to `buggy/`, `fixed/`, `diff` files |

## Pattern Families

Each case belongs to one of 9 pattern families, which maps to a subset of Spectre's rules:

| Family | Description | Example Bugs |
|--------|-------------|--------------|
| `auth` | Authorization and access control | Missing signatory, overprivileged controller, unilateral archive |
| `visibility` | Party visibility and disclosure | Missing observer, accidental disclosure, observer set bloat |
| `temporal` | Time handling and deadlines | Deadline bypass, phase ordering violation, time window misconfiguration |
| `lifecycle` | Contract lifecycle management | Stale references after archive, orphaned contracts, missing cleanup |
| `invariant` | Data validation and consistency | Asymmetric validation, data inconsistency, missing ensure clauses |
| `scalability` | Performance and resource bounds | Unbounded contract creation, observer set growth |
| `upgrade` | Package versioning and compatibility | Interface compatibility, version mismatch |
| `integration` | Cross-system integration | API spec non-conformance, missing backends |
| `diagnostics` | Error messages and observability | Misleading errors, indistinguishable audit records, dead code |

## Evaluation Methodology

The benchmark uses **differential analysis**:

1. Parse and analyze the **buggy** version of each case
2. Parse and analyze the **fixed** version of each case
3. Filter findings to only rules matching the case's `pattern_family`
4. Classify:
   - **True Positive (TP)**: `good_for_static=true` AND findings in buggy > findings in fixed
   - **True Negative (TN)**: `good_for_static=false` AND no relevant findings in buggy
   - **False Positive (FP)**: `good_for_static=false` AND relevant findings in buggy
   - **False Negative (FN)**: `good_for_static=true` AND findings in buggy <= findings in fixed

Metrics:
- **Precision** = TP / (TP + FP)
- **Recall** = TP / (TP + FN)
- **F1** = 2 * Precision * Recall / (Precision + Recall)

## Data Sources

| Source | Cases | Evidence | Description |
|--------|-------|----------|-------------|
| Temple DAML (Halborn) | 8 | Strong | Professional security audit of Temple DAML contracts |
| Temple Diff (Halborn) | 4 | Strong | Differential review of Temple contract changes |
| Obsidian AMM (Halborn) | 12 | Strong | Professional security audit of Obsidian Tradecraft AMM |
| Pattern-derived | 9 | Synthetic | Hand-crafted positive/negative pairs for each rule family |
| Canton releases | 4 | Moderate | Bugs extracted from Canton GitHub release notes |
| DAML releases | 1 | Moderate | Bugs extracted from DAML SDK release notes |
