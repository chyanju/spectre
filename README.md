<p align="center">
  <img src="assets/spectre.png" alt="Spectre" width="160" />
</p>

<h1 align="center">Spectre</h1>

<p align="center">
  A static security analysis tool for <a href="https://www.digitalasset.com/developers">DAML</a> smart contracts.<br/>
  Spectre parses DAML source files into a security-focused AST, runs configurable pattern-based inspections, and reports findings with source locations and suggested fixes.
</p>

## Benchmark Results

Evaluated against a 38-case labeled dataset of real audit findings, synthetic patterns, and release-note bugs:

| Metric | Value |
|--------|-------|
| Precision | 100% |
| Recall | 97% |
| F1 Score | 98% |
| True Positives | 28 |
| True Negatives | 17 |
| False Positives | 0 |
| False Negatives | 1 |

The 1 false negative involves a pattern that is out of scope for static pattern matching.

## Requirements

- [Stack](https://docs.haskellstack.org/) (build tool)
- GHC 9.4.x (managed by Stack, resolver `lts-21.25`)

## Build

```bash
stack build
```

## Quick Start

Run Spectre against a sample file from the included benchmark dataset:

```bash
# Detect a temporal phase-separation bug in a real audit case
stack exec spectre -- analyze benchmarks/audit-derived/obsidian/OBS-HAL-06/buggy/SettlementDeadline.daml

# Same file, but with JSON output
stack exec spectre -- analyze --json benchmarks/audit-derived/obsidian/OBS-HAL-06/buggy/SettlementDeadline.daml

# Detect a missing role-check bug in a synthetic example
stack exec spectre -- analyze benchmarks/pattern-derived/DOC-AUTH-01/buggy/TokenAdmin.daml
```

Replace the path with your own `.daml` files to analyze your contracts.

## Usage

### Analyze DAML files

```bash
# Analyze one or more DAML files
stack exec spectre -- analyze file1.daml file2.daml

# JSON output
stack exec spectre -- analyze --json file1.daml

# Enable only specific rules (repeatable flag)
stack exec spectre -- analyze --rule SPEC-AUTH-001 --rule SPEC-VIS-001 file1.daml

# Disable specific rules
stack exec spectre -- analyze --disable SPEC-DIAG-004 file1.daml

# Verbose output
stack exec spectre -- analyze -v file1.daml
```

### Run benchmarks

```bash
# Evaluate against the labeled dataset
stack exec spectre -- benchmark --dir benchmarks -v
```

### List available rules

```bash
# Print all 38 inspection rules with IDs, severities, and descriptions
stack exec spectre -- rules
```

## Test

```bash
stack test
```

## Rules

Spectre ships 38 rules across 10 modules covering DAML-specific security concerns:

| Module | Rules | Examples |
|--------|-------|---------|
| Authorization | 2 | Unilateral archive, missing role check |
| Visibility | 2 | Controller not observer, asymmetric party |
| Temporal | 3 | Missing deadline, off-by-one time, getTime misuse |
| Invariant | 13 | Missing validation, partial functions, floating-point equality |
| State | 2 | Set.insert without guard, asymmetric add/remove |
| Lifecycle | 4 | NonConsuming creates, missing consuming choice, split remainder |
| Scalability | 3 | Unbounded growth, event-as-contract bloat |
| CrossTemplate | 6 | Duplicate errors, misleading names, missing audit trail |
| Upgrade | 2 | Raw ContractId return, interface view field drop |
| Integration | 1 | Unchecked expected parameter |

See [docs/rules.md](docs/rules.md) for the full catalog.

## Architecture

```
DAML Source → Parser (Megaparsec) → AST → Analysis Engine → Rules → Findings → Report
```

The parser produces a DAML-specific AST with `Template`, `Choice`, and `PartyExpr` as first-class nodes. Each rule is a pure function `Module -> [Finding]`. The analysis engine filters rules by configuration and runs them against every parsed module.

See [docs/architecture.md](docs/architecture.md) for details.

## Project Structure

```
spectre/
├── assets/spectre.png           # Project logo
├── app/Main.hs                  # CLI entry point
├── src/Spectre/
│   ├── Ast.hs                   # DAML AST types
│   ├── Parser.hs                # Parser entry point
│   ├── Parser/
│   │   ├── Lexer.hs             # Whitespace / comment handling
│   │   ├── Expr.hs              # Expression / pattern / type parsers
│   │   └── Template.hs          # Template / choice / decl parsers
│   ├── Analysis.hs              # Analysis engine
│   ├── Inspection.hs            # Finding / Inspection types
│   ├── Config.hs                # Configuration
│   ├── Report.hs                # Human-readable and JSON output
│   ├── Benchmark.hs             # Benchmark evaluation harness
│   └── Rules/
│       ├── All.hs               # Rule registry
│       ├── Authorization.hs
│       ├── Visibility.hs
│       ├── Temporal.hs
│       ├── Invariant.hs
│       ├── State.hs
│       ├── Lifecycle.hs
│       ├── Scalability.hs
│       ├── CrossTemplate.hs
│       ├── Upgrade.hs
│       └── Integration.hs
├── test/Spec.hs                 # Test suite (13 tests)
├── benchmarks/                  # 38-case labeled dataset
├── package.yaml                 # hpack project config
├── stack.yaml                   # Stack resolver config
└── LICENSE                      # MIT
```

## Documentation

- [Rules Catalog](docs/rules.md) — All 38 rules with IDs, severities, and descriptions
- [Benchmark Dataset](docs/benchmarks.md) — Structure, sources, schema, and evaluation methodology
- [Architecture](docs/architecture.md) — Parser, AST, analysis engine, and data flow

## License

MIT - see [LICENSE](LICENSE) for details.
