# Phase 1 FHIRPath Expression Workarounds

**Date:** 2026-05-04
**Branch:** exclusions-phase-1
**Context:** eRSD Phase 1 triggering optimization — FHIRPath expressions evaluated by opencds CqlProcessor (FHIRPath-via-CQL translation)

## Summary

The Phase 1 PlanDefinition's FHIRPath condition expressions hit multiple bugs in the opencds CQL engine's FHIRPath-to-CQL translation layer. This document catalogs each bug, the workaround applied, and the recommended upstream fix.

All workarounds are applied automatically by `scripts/rewrite_fhirpath.py` to the source PlanDefinition before building the test KAR bundle.

## Workarounds Applied

### 1. Choice-type narrowing: `.ofType(T)` → `(X as FHIR.T)`

**Bug:** `SystemMethodResolver.createOfType` translates `.ofType(T)` into a CQL Query that retains the source's choice type (`choice<Period, Timing, dateTime, instant>`) instead of narrowing to `list<T>`. Downstream calls to `.exists()`, `.empty()`, `.count()` then fail with overload ambiguity.

**Workaround:** Replace `.ofType(T)` with `(X as FHIR.T)`. The `as` cast produces a direct ELM `As` node with the narrowed type.

| Original | Rewritten |
|---|---|
| `effective.ofType(dateTime).exists().not()` | `(effective as FHIR.dateTime) is null` |
| `effective.ofType(dateTime) >= bound` | `(effective as FHIR.dateTime) >= bound` |
| `effective.ofType(Period).start` | `(effective as FHIR.Period).start` |
| `onset.ofType(dateTime).exists().not()` | `(onset as FHIR.dateTime) is null` |
| `value.ofType(CodeableConcept).coding...` | `(value as FHIR.CodeableConcept).coding...` |
| `value.ofType(string).exists().not()` | `(value as FHIR.string) is null` |
| `value.ofType(string).lower()...` | `(value as FHIR.string).lower()...` |

**Upstream fix:** `SystemMethodResolver.createOfType` should emit a narrowed result type on the Query so that method chains resolve correctly.

### 2. FHIRPath `|` union → CQL `Union(String, String)` failure

**Bug:** FHIRPath's `code in ('entered-in-error' | 'refuted')` uses `|` as a union operator between string literals. The CQL translator emits `Union(System.String, System.String)` which has no overload — CQL's `Union` expects `Union(List, List)`.

**Workaround:** Replace `code in ('a' | 'b')` with `code = 'a' or code = 'b'`.

| Original | Rewritten |
|---|---|
| `code in ('entered-in-error' \| 'refuted')` | `code = 'entered-in-error' or code = 'refuted'` |

**Upstream fix:** The FHIRPath-to-CQL translator should wrap string literals in singleton lists before emitting `Union`, or translate the `in` + `|` pattern to CQL `in { 'a', 'b' }`.

### 3. Duration arithmetic: `1 day * %integerParam` fails at runtime

**Bug:** When integer parameters are bound as HAPI `IntegerType` (FHIR.integer), the CQL engine's `Multiply(Quantity, IntegerType)` path throws `CqlException: Cannot cast IntegerType as Quantity`. The `CqlFhirParametersConverter.toCqlType()` should unwrap `IntegerType` → `java.lang.Integer`, but the engine's runtime arithmetic doesn't apply `FHIRHelpers.ToInteger` on the bound value.

**Workaround:** Inline duration literals directly in expressions, eliminating the multiplication.

| Original | Rewritten |
|---|---|
| `1 day * %normalReportingDuration` | `14 days` |
| `1 day * %dxTimeboxDuration` | `30 days` |
| `1 day * %labTimeboxDuration` | `30 days` |
| `+ %normalReportingDuration +` | `+ 14 days +` |
| `+ %ambulatoryReportingDuration +` | `+ 1 day +` |

**Upstream fix:** The engine's `ToQuantityEvaluator` / arithmetic paths should auto-unwrap HAPI `IntegerType`/`DecimalType` to Java primitives, or the `CqlFhirParametersConverter` should ensure bound values are always CQL-native types.

### 4. `context Patient` with no subject ID

**Bug:** `LibraryConstructor.constructContext(null)` always generates `context Patient` in the synthesized CQL. When no patient ID is passed to `CqlProcessor.evaluate()`, the engine's Patient context retrieval fails silently.

**Workaround:** Pass the patient ID from `NotificationContext` as the first argument to `evaluate()`.

```java
String patientId = kd.getNotificationContext() != null
    ? kd.getNotificationContext().getPatientId() : null;
newEvaluator().evaluate(patientId, expression, params, ...);
```

**Upstream fix:** `LibraryConstructor` should accept a context type parameter and use `context Unfiltered` when no subject is needed, or the `CqlProcessor.evaluate()` overload should allow specifying context.

### 5. DiagnosticReport field mismatch (Phase 1 expression bug)

**Bug:** The Phase 1 `is-encounter-reportable` expression applied `value.ofType(CodeableConcept)` and `value.ofType(string)` predicates to `%diagnosticResultValues`, which is typed as `List<FHIR.DiagnosticReport>`. DiagnosticReport has no `value[x]` or `interpretation` field. Copy-paste error from the Observation branch.

**Fix:** Removed value/interpretation predicates from the `%diagnosticResultValues.where(...)` block, keeping only effective/period date filters.

### 6. Unused parameter stripping

**Observation:** Resolved PD-level variables (`%normalReportingDuration`, `%dxTimeboxDuration`, etc.) are added to the Parameters map for every action evaluation, even when the expression doesn't reference them. Each unused parameter generates a CQL `parameter` declaration. While not causing errors after the other fixes, this adds unnecessary compilation overhead.

**Fix:** Before evaluation, strip parameters whose names don't appear in the expression text.

### 7. `relatedAction` loop (Phase 1 PD modeling bug)

**Bug:** `is-encounter-in-progress` and `is-amb-encounter-in-progress` carried `relatedAction → check-reportable before-start 6h` which didn't exist in the original PD. Under `ignore.timers=true` (test configuration), the 6h offset collapsed to immediate re-execution, causing infinite recursion.

**Fix:** Removed the `relatedAction` blocks from both actions in the source Phase 1 PD.

## Files Changed

### eCRNow (this project)

| File | Change |
|---|---|
| `FhirPathProcessor.java` | Patient context fix (#4), unused param stripping (#6), improved error logging (extracts OperationOutcome from evaluation error) |
| `BsaServiceUtils.java` | No change (empty param binding restored to original) |
| `pom.xml` | Added `kotlin.version=2.1.20` (required by CQF 4.5.1's antlr-kotlin-runtime) |
| `eRSD-RuleFilter-bundle.json` | Rebuilt trimmed KAR with rewritten Phase 1 PD |
| `scripts/rewrite_fhirpath.py` | Automated FHIRPath rewrites (#1-3) applied to source PD before KAR build |
| `scripts/build_trimmed_kar.py` | Builds trimmed KAR from v3 spec bundle + Phase 1 PD + supplemental VSACs |

### aphl-ersd-specifications-v3 (upstream PD)

| File | Change |
|---|---|
| `plandefinition-us-ecr-specification-phase1.json` | relatedAction removal (#7), DiagnosticReport fix (#5); FHIRPath rewrites applied by script |

## Recommended Upstream Changes (clinical-reasoning / cql-engine)

These would eliminate the need for workarounds #1-4:

1. **`SystemMethodResolver.createOfType`** — Emit narrowed result type on the Query so `.ofType(T).exists()` resolves to `Exists(list<T>)`, not `Exists(choice<...>)`.

2. **FHIRPath `|` to CQL translation** — Translate `X in ('a' | 'b')` to `X in { 'a', 'b' }` rather than `Union(String, String)`.

3. **FHIR primitive auto-unwrapping in arithmetic** — `ToQuantityEvaluator` and arithmetic operators should recognize HAPI `IntegerType`/`DecimalType` and auto-unwrap to Java primitives, or `CqlFhirParametersConverter` should ensure parameters are always CQL-native.

4. **`LibraryConstructor` context handling** — Accept an optional context type; default to `Unfiltered` when no subject is provided instead of always emitting `context Patient`.

5. **CQL compiler: reject `DateTime + Integer` (no unit)** — Currently compiles but throws `InvalidPrecision: 1` at runtime. Should be a compile-time error.

6. **Typed choice accessors** — FHIR ModelInfo should expose `effectiveDateTime`, `valueString`, `onsetDateTime` as properties so FHIRPath authors can use them. Currently only `effective.ofType(dateTime)` / `(effective as FHIR.dateTime)` work.
