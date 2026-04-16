# Spectre Rules Catalog

Spectre ships 38 inspection rules across 10 modules. Each rule has a unique ID, severity level, and targets a specific DAML security concern.

## Severity Levels

| Severity | Meaning |
|----------|---------|
| **Error** | Likely bug or security vulnerability that will cause runtime failure or access control bypass |
| **Warning** | Potential security issue or anti-pattern that should be reviewed |
| **Info** | Code quality observation that may indicate deeper issues |

## Summary

| Severity | Count |
|----------|-------|
| Error | 2 |
| Warning | 31 |
| Info | 5 |

---

## Authorization (2 rules)

| ID | Name | Severity | Description |
|----|------|----------|-------------|
| SPEC-AUTH-001 | Signatory allows unilateral archive | Warning | Template signatory model allows a single actor to unilaterally archive records. Consider requiring a governance party as signatory to protect audit trails. |
| SPEC-AUTH-002 | Missing role membership check | Error | Choice controller is a parameter but the body does not verify role membership (e.g., Set.member check). Any party who can exercise this choice may bypass role-based access control. |

## Visibility (2 rules)

| ID | Name | Severity | Description |
|----|------|----------|-------------|
| SPEC-VIS-001 | Controller not listed as observer | Error | A choice controller is not listed as either a signatory or observer of the template. The controller cannot see this contract and therefore cannot exercise this choice. |
| SPEC-VIS-002 | Missing symmetric party in observer clause | Warning | Template observers/signatories reference one side of a party pair (e.g., sender) but not the counterpart (e.g., receiver), creating a visibility gap. |

## Temporal (3 rules)

| ID | Name | Severity | Description |
|----|------|----------|-------------|
| SPEC-TEMP-001 | Missing deadline assertion | Warning | Choice references time-related fields but does not assert that the current time satisfies deadline constraints. |
| SPEC-TEMP-002 | Potentially off-by-one time comparison | Info | Time comparison uses '<=' which may allow execution at the exact deadline moment. Consider whether '<' is more appropriate. |
| SPEC-TEMP-003 | getTime result stored in contract field | Warning | getTime returns preparation-time in Canton, not sequencer time. Storing it in a contract field causes incorrect time arithmetic under MPC/multi-sig signing. |

## Invariant (13 rules)

| ID | Name | Severity | Description |
|----|------|----------|-------------|
| SPEC-INV-001 | Missing input validation | Warning | Choice parameter is used in a create statement without prior assertion/validation. User-controlled input should be validated before use. |
| SPEC-INV-002 | Unsafe partial function | Warning | Use of partial function (head, tail, !!) that will crash on empty input. Use safe alternatives or add a guard. |
| SPEC-INV-003 | Floating-point equality comparison | Warning | Equality comparison (==) on numeric expressions that may involve floating-point arithmetic. Consider using an epsilon-based comparison. |
| SPEC-INV-004 | Asymmetric validation across choices | Warning | Some choices validate template fields while sibling choices do not, creating inconsistent invariant enforcement. |
| SPEC-INV-005 | Unused fetch result | Warning | A fetch result is discarded or bound to an unused variable. The fetched contract data should be validated against choice parameters. |
| SPEC-INV-006 | Fail-open Optional pattern | Warning | A case expression on an Optional value silently succeeds in the None branch (pure ()), meaning the absence of configuration is ignored rather than rejected. |
| SPEC-INV-007 | Self-assignment without change guard | Warning | A choice parameter is assigned directly to a template field via 'create this with { field = param }' without checking that the new value differs from the old. |
| SPEC-INV-008 | Missing upper-bound validation | Warning | A numeric parameter is validated with a lower bound check but no upper bound check. Extreme values may break business logic. |
| SPEC-INV-009 | Asymmetric validation between sibling functions | Warning | Sibling functions (sharing an operation suffix) have asymmetric assertMsg checks -- one is missing invariant validations present in the other. |
| SPEC-INV-010 | Missing cross-entity validation | Warning | Multiple contracts are fetched and their fields are used together in a create, but no assertion checks consistency between the fetched entities. |
| SPEC-INV-011 | Time field not validated in ensure clause | Warning | Template has a Time-typed field but the ensure clause does not reference it, potentially allowing contracts with invalid time relationships. |
| SPEC-INV-012 | Indirect parameter assignment without guard | Warning | A choice parameter is wrapped in Some and assigned to a field via a let binding, but there is no assertion that the new value differs from the old or that the existing state is None. |
| SPEC-INV-013 | Uncanonicalized list persisted in created contract | Warning | Data flows to both a processing function and a create statement without canonicalization, creating a potential mismatch between processed and stored state. |

## State (2 rules)

| ID | Name | Severity | Description |
|----|------|----------|-------------|
| SPEC-STATE-001 | Set.insert without membership guard | Warning | Choice calls Set.insert without first checking Set.member. This can lead to silent no-op state updates or duplicate entries. |
| SPEC-STATE-002 | Asymmetric add/remove operations | Info | Template has choices that add to a collection but no corresponding choices that remove from it, potentially leading to unbounded growth. |

## Lifecycle (4 rules)

| ID | Name | Severity | Description |
|----|------|----------|-------------|
| SPEC-LIFE-001 | NonConsuming choice creates contracts | Warning | A nonconsuming choice creates new contracts but has no guard against repeated execution. This could lead to unbounded contract creation. |
| SPEC-LIFE-002 | Multi-signatory template lacks consuming choice | Warning | Template with multiple signatories has no consuming choice. Archive requires unanimous consent, preventing unilateral revocation. |
| SPEC-LIFE-003 | Split without remainder handling | Warning | A consuming choice archives a contract and re-creates it with a partial amount but does not handle the remainder. The difference is effectively lost. |
| SPEC-LIFE-004 | Consuming choice creates different template type | Warning | A consuming choice archives the current contract and creates a contract of a different template type. ContractId references from other contracts will become dangling. |

## Scalability (3 rules)

| ID | Name | Severity | Description |
|----|------|----------|-------------|
| SPEC-SCALE-001 | Unbounded collection growth | Warning | Template has a collection field (list/set) that is only ever appended to but never pruned. This will grow without bound over time. |
| SPEC-SCALE-002 | Event-as-contract bloat | Warning | Template has no choices. Contracts of this type can only be archived by signatories and will accumulate in the active contract set. |
| SPEC-SCALE-003 | Unbounded iteration with create | Warning | A list is iterated (forA/mapA/traverse) with a callback that calls create, but there is no assertion on the list length. |

## CrossTemplate / Diagnostics (6 rules)

| ID | Name | Severity | Description |
|----|------|----------|-------------|
| SPEC-DIAG-001 | Duplicate error messages | Info | Multiple assertMsg calls use identical error messages, making it difficult to identify which assertion failed. |
| SPEC-DIAG-002 | Duplicate event discriminator | Warning | Sibling choices create the same event type with the same action/discriminator literal, making audit events indistinguishable. |
| SPEC-DIAG-003 | Misleading assertMsg message | Info | An assertMsg error message describes the success condition rather than the failure. When the assertion fails, operators see a misleading message. |
| SPEC-DIAG-004 | Unused top-level definition | Info | A top-level function or value is defined but not referenced elsewhere in the module. Dead code increases maintenance burden. |
| SPEC-DIAG-005 | Misleading choice name | Warning | A consuming choice has a name suggesting read-only/validation semantics but its body performs state mutations. |
| SPEC-DIAG-006 | Consuming cancel/reject choice without audit trail | Warning | A consuming choice with cancellation/rejection semantics has a trivial body (pure ()) -- the contract is archived without recording a reason or creating an audit record. |

## Upgrade (2 rules)

| ID | Name | Severity | Description |
|----|------|----------|-------------|
| SPEC-UPGRADE-001 | Choices return raw ContractId of own template | Warning | Multiple choices return `ContractId <TemplateName>` directly instead of a wrapped result type. This prevents non-breaking upgrades because changing a choice return type is a breaking change in DAML. |
| SPEC-UPGRADE-002 | Interface view drops template fields | Warning | A template field is not referenced in the interface view expression. Consumers fetching via the interface will receive incomplete data. |

## Integration (1 rule)

| ID | Name | Severity | Description |
|----|------|----------|-------------|
| SPEC-INTEG-001 | Unchecked 'expected' parameter | Warning | A choice parameter prefixed with 'expected' is not validated against the corresponding template field. A caller could pass a mismatched value. |
