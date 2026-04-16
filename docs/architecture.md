# Architecture

Spectre follows a straightforward pipeline architecture:

```
DAML Source Files
       |
       v
   Parser (Megaparsec)
       |
       v
   AST (Module)
       |
       v
   Analysis Engine
       |
       +--- Inspection Rules (38 rules, 10 modules)
       |
       v
   Findings
       |
       v
   Report (Human-readable / JSON)
```

## Components

### CLI (`app/Main.hs`)

The entry point provides three subcommands via `optparse-applicative`:

| Command | Purpose |
|---------|---------|
| `analyze` | Parse DAML files and report security findings |
| `benchmark` | Evaluate against the labeled dataset, compute precision/recall/F1 |
| `rules` | List all available inspection rules |

The `analyze` command exits with code 1 if any `Error`-severity findings are detected, making it suitable for CI integration.

### Parser (`src/Spectre/Parser.hs`)

A full-file Megaparsec parser for DAML, split into three sub-modules:

| Module | Responsibility |
|--------|---------------|
| `Parser.Lexer` | Whitespace handling, comment skipping, indentation-aware combinators |
| `Parser.Template` | Module headers, imports, top-level declarations, template/choice syntax |
| `Parser.Expr` | Expressions, patterns, types, do-blocks |

Entry points:
- `parseDamlFile :: FilePath -> IO (Either ParseError Module)` -- reads from disk
- `parseDaml :: FilePath -> Text -> Either ParseError Module` -- pure parse

### AST (`src/Spectre/Ast.hs`)

A DAML-specific AST designed for security analysis. Unlike a generic Haskell AST, it makes DAML constructs first-class:

| Type | Description |
|------|-------------|
| `Module` | Top-level unit: module name, imports, declarations |
| `Decl` | Declaration variants: `DTemplate`, `DDataType`, `DFunction`, `DTypeSig`, `DClass`, `DInstance`, `DOther` |
| `Template` | Template with fields, signatory, observer, ensure clause, choices, key, interface implementations |
| `Choice` | Choice with name, consuming semantics, controller, parameters, body statements |
| `Stmt` | Do-block statements: bind, let, create, exercise, fetch, archive, assert, return, raw expression |
| `Expr` | Expression tree with 16 variants (var, app, if, case, do, record, field access, etc.) |
| `PartyExpr` | Party expressions for signatory/observer/controller clauses |
| `SrcSpan` | Source location attached to every node for error reporting |

### Inspection Types (`src/Spectre/Inspection.hs`)

The types that connect rules to the analysis engine:

| Type | Description |
|------|-------------|
| `Inspection` | Rule definition: ID, name, description, severity, categories, and check function |
| `Finding` | A triggered inspection: rule ID, severity, source location, message, suggestion, template/choice context |
| `InspectionId` | Newtype over `Text` (e.g., `"SPEC-AUTH-001"`) |
| `Severity` | `Info`, `Warning`, `Error` (ordered for sorting) |
| `Category` | `Authorization`, `Visibility`, `Temporal`, `Invariant`, `StateIntegrity`, `Lifecycle`, `Scalability`, `CrossTemplate`, `Diagnostics` |

Each `Inspection` carries a check function:

```haskell
inspCheck :: Module -> [Finding]
```

Rules are pure functions from AST to findings -- no IO, no state.

### Analysis Engine (`src/Spectre/Analysis.hs`)

The engine is a simple filter-then-map architecture:

1. Filter `allInspections` by `Config.isRuleEnabled`
2. For each enabled inspection, for each module, call `inspCheck`
3. Concatenate and sort findings by severity (descending)

```haskell
analyze :: Config -> [Module] -> AnalysisResult
```

### Rule Modules (`src/Spectre/Rules/`)

10 modules, each exporting a `[Inspection]` list:

| Module | Rules | Focus |
|--------|-------|-------|
| `Authorization.hs` | 2 | Signatory model, role-based access control |
| `Visibility.hs` | 2 | Observer/controller visibility |
| `Temporal.hs` | 3 | Time handling, deadlines, phase separation |
| `Invariant.hs` | 13 | Input validation, data consistency |
| `State.hs` | 2 | Collection operations, state transitions |
| `Lifecycle.hs` | 4 | Contract creation/archival patterns |
| `Scalability.hs` | 3 | Resource bounds, contract bloat |
| `CrossTemplate.hs` | 6 | Error messages, naming, audit trails |
| `Upgrade.hs` | 2 | Version compatibility, interface views |
| `Integration.hs` | 1 | Cross-system parameter validation |

`Rules.All` aggregates all modules into a single `allInspections :: [Inspection]` registry.

### Configuration (`src/Spectre/Config.hs`)

| Field | Type | Description |
|-------|------|-------------|
| `cfgEnabledRules` | `Maybe (Set Text)` | `Nothing` = all rules enabled; `Just set` = only these |
| `cfgDisabledRules` | `Set Text` | Rules to exclude |
| `cfgOutputFormat` | `OutputFormat` | `HumanReadable` or `JsonOutput` |
| `cfgVerbose` | `Bool` | Verbose output |
| `cfgBenchmarkDir` | `Maybe FilePath` | Benchmark dataset directory |

### Report (`src/Spectre/Report.hs`)

Two output formats:

- **Human-readable**: Header with file/rule counts, each finding with severity icon + source location + message + suggestion, summary line
- **JSON**: Pretty-printed JSON via `aeson` for tool integration

### Benchmark Harness (`src/Spectre/Benchmark.hs`)

The benchmark subsystem:

1. Discovers cases by walking `audit-derived/`, `pattern-derived/`, `github-derived/` subdirectories
2. For each case, reads `meta.json`, parses buggy and fixed DAML files
3. Runs analysis on both versions
4. Uses `patternFamilyRules` to filter findings to only the relevant rule subset for each case's `pattern_family`
5. Classifies as TP/TN/FP/FN based on differential finding counts and the `good_for_static` label
6. Computes aggregate precision, recall, and F1

## Data Flow

```
                          CLI (Main.hs)
                         /      |       \
                        v       v        v
                  runAnalyze  runBench  listRules
                       |        |
           +-----------+        |
           v                    v
   parseDamlFile          runBenchmark
   (Parser.hs)           (Benchmark.hs)
           |               /        \
           v              v          v
        Module        parseDaml   parseDaml
        (Ast.hs)      (buggy)     (fixed)
           |               \        /
           v                v      v
        analyze           analyze  analyze
      (Analysis.hs)     (Analysis.hs)
           |                   |
           |    +--- inspCheck per rule ---+
           |    |  Rules/Authorization     |
           |    |  Rules/Visibility        |
           |    |  Rules/Temporal          |
           |    |  Rules/Invariant         |
           |    |  Rules/State             |
           |    |  Rules/Lifecycle         |
           |    |  Rules/Scalability       |
           |    |  Rules/CrossTemplate     |
           |    |  Rules/Upgrade           |
           |    |  Rules/Integration       |
           |    +--------------------------+
           v                   |
     AnalysisResult      AnalysisResult (per buggy/fixed)
     [Finding]                 |
           |                   v
           v            filterRelevant + classify TP/TN/FP/FN
     renderFindings            |
     (Report.hs)               v
           |              computeMetrics
           v                   |
     Text output               v
     (human/JSON)        Metrics output
```
