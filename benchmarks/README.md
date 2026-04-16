# DAML Bug Benchmarks

A curated dataset of DAML smart contract bugs, vulnerabilities, and anti-patterns for evaluating automated security analysis tools (static analysis, symbolic execution, fuzzing).

## Motivation

Unlike EVM where datasets like SmartBugs/SWC exist, there is no public benchmark for DAML-specific security issues. DAML's unique authorization model (signatories/observers/controllers), privacy guarantees (disclosure rules), and ledger model (contract creation/archival, no mutable state) produce bug classes that are fundamentally different from traditional smart contract vulnerabilities.

This dataset fills that gap by collecting real-world and synthetic DAML bug evidence from:
- **Professional security audits** (Halborn reports on Temple, Obsidian)
- **GitHub release notes** (Canton and Daml SDK bugfix evidence)
- **Documentation-derived patterns** (positive/negative code pairs from best practice docs)

## Directory Structure

```
daml-bug-benchmarks/
├── README.md                          # This file
├── schema.json                        # Unified meta.json field definitions (JSON Schema)
├── corpus.json                        # Master index of all cases
├── audit-derived/                     # Cases from professional security audits
│   ├── temple/                        # Temple DAML Contracts – Halborn (8 findings)
│   ├── temple-diff/                   # Temple Diff Review – Halborn (4 findings)
│   └── obsidian/                      # Obsidian Tradecraft AMM – Halborn (12 findings)
├── github-derived/                    # Cases from GitHub release notes / issues
│   ├── canton-releases/
│   └── daml-releases/
├── pattern-derived/                   # Synthetic positive/negative code pairs
└── public-corpus/                     # Unlabeled public DAML code for analysis
```

## Schema

Every case has a `meta.json` conforming to `schema.json`. Key fields:

| Field | Description |
|---|---|
| `id` | Unique ID (e.g. `TEMPLE-HAL-03`, `OBS-HAL-01`) |
| `pattern_family` | One of: `auth`, `visibility`, `temporal`, `lifecycle`, `invariant`, `scalability`, `upgrade`, `integration`, `diagnostics` |
| `severity` | `critical`, `high`, `medium`, `low`, `informational` |
| `evidence_strength` | `strong` (audit-confirmed), `moderate` (review/release-note), `weak` (inferred), `synthetic` |
| `oracle` | Detection hints: `static_signal` for static analysis, `symbolic_property` for symbolic execution |
| `good_for_static` | Whether suited for static analysis tool evaluation |
| `good_for_symbolic` | Whether suited for symbolic execution tool evaluation |

## Pattern Families

| Family | DAML-Specific Bugs |
|---|---|
| **auth** | Missing signatory, overprivileged controller, unilateral archive of shared state |
| **visibility** | Missing observer, accidental disclosure, observer set bloat |
| **temporal** | Deadline bypass, phase ordering violation, time window misconfiguration |
| **lifecycle** | Stale references after archive, missing cleanup, orphaned contracts |
| **invariant** | Asymmetric validation, data inconsistency, missing ensure clauses |
| **scalability** | Unbounded contract creation, observer set growth, no rate limiting |
| **upgrade** | Package ID mismatch, interface compatibility, source vs binary inclusion |
| **integration** | Spec non-conformance (CIP-0056), missing API backends |
| **diagnostics** | Misleading errors, indistinguishable audit records, dead code |

## Data Sources

### Audit Reports (Phase 1)
1. **Temple DAML Contracts** – Halborn, public. 8 findings.
2. **Temple Diff Review** – Halborn, public. 4 findings.
3. **Obsidian Tradecraft AMM** – Halborn, public. 12 findings.

### GitHub-Derived (Phase 2)
- Canton release notes (digital-asset/canton)
- Daml SDK release notes (digital-asset/daml)

### Pattern-Derived (Phase 3)
- Synthetic code pairs constructed from Daml documentation and best practice guides. Currently contains **22 cases** covering all 9 pattern families, filling gaps in visibility, scalability, integration, and numeric precision loss.

## Usage

```python
import json, glob

# Load all cases
cases = []
for meta_path in glob.glob("**/meta.json", recursive=True):
    with open(meta_path) as f:
        cases.append(json.load(f))

# Filter for static-analysis-suitable cases
static_cases = [c for c in cases if c.get("good_for_static")]

# Group by pattern family
from collections import Counter
family_dist = Counter(c["pattern_family"] for c in cases)
```

## License

The benchmark metadata and synthetic code samples are provided for research purposes. Original audit report content is attributed to Halborn. Code snippets from public repositories retain their original licenses.
