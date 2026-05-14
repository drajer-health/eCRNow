package com.drajer.bsa.controller;

import static com.drajer.bsa.controller.ExpectedOutcome.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import java.util.Arrays;
import java.util.Collection;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.springframework.test.context.TestPropertySource;

/**
 * Phase I Triggering Optimization Test Suite
 *
 * <p>Tests for eRSD triggering optimization Phase I exclusion categories based on the RCKMS/eRSD
 * Investigation (Feb 2026):
 *
 * <ul>
 *   <li>Negative lab result values (SNOMED 260385009, 260415000, text "Negative"/"Not detected")
 *   <li>Uncoded/text-only lab results
 *   <li>Diagnosis timeboxing (diagnosis > 30 days before encounter)
 *   <li>Lab result timeboxing (lab result > 30 days before encounter start)
 *   <li>Refuted diagnoses (verificationStatus = refuted)
 *   <li>Mixed results (negative + positive on same patient — should NOT be excluded)
 *   <li>Neg-exempt conditions (e.g., Gonorrhea — negative results still reportable)
 * </ul>
 *
 * <p>Expected outcomes reflect Phase 1 PD behavior: negative-value, refuted-status, and timebox
 * filters exclude matching scenarios (NOT_TRIGGERED). Positive controls and mixed-result cases
 * still REPORT.
 *
 * <p>Phase 2 carry-over: neg-exempt conditions (Gonorrhea/Hep C reportable on negative
 * results). Phase 1 has no exemption logic — see {@code phase1-neg-exempt-condition}
 * comment and {@code docs/phase1-fhirpath-workarounds.md}.
 */
@RunWith(Parameterized.class)
@TestPropertySource(
    properties = {
      "kar.directory=src/test/resources/Bsa/Scenarios/kars/rulefilters",
      "cql.enabled=false",
      "bsa.kar.activate=true"
    })
public class Phase1TriggeringOptimizationTest extends BaseKarsTest {
  protected FhirContext fhirContext = FhirContext.forCached(FhirVersionEnum.R4);

  private static final String PLAN_DEF_FOLDER = "PlanDefinition_eRSD_Instance_Example";
  private static final String PLAN_DEF_URL =
      "http://ersd.aimsplatform.org/fhir/PlanDefinition/us-ecr-specification";

  public Phase1TriggeringOptimizationTest(TestCaseInfo testCaseInfo) {
    super(testCaseInfo);
  }

  @Test
  public void test() throws Exception {
    super.testScenarioAndValidate();
  }

  // -----------------------------------------------------------------------
  // Expected outcomes reflect Phase 1 PD behavior. Scenarios that hit a
  // Phase 1 exclusion filter (negative value, timebox exceeded, refuted
  // verification status) expect NOT_TRIGGERED. Positive controls, mixed
  // results, and pass-through cases expect REPORTED.
  // -----------------------------------------------------------------------
  @Parameters(name = "{0}")
  public static Collection<TestCaseInfo> data() {
    return Arrays.asList(
        // 1. POSITIVE CONTROL — Positive coded lab result, within encounter window.
        //    Should ALWAYS trigger, both pre- and post-Phase I.
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-positive-lab-control",
            REPORTED),

        // 2. NEGATIVE LAB (SNOMED 260385009) — Chlamydia test with Negative result.
        //    Pre-Phase I: triggers (code matches lrtc).
        //    Post-Phase I: should be NOT_TRIGGERED (negative value excluded).
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-negative-lab-snomed",
            NOT_TRIGGERED), // Phase 1: excluded by negative-value / timebox / refuted-status filter

        // 3. NOT DETECTED LAB (SNOMED 260415000) — Chlamydia test with Not Detected result.
        //    Pre-Phase I: triggers.
        //    Post-Phase I: should be NOT_TRIGGERED.
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-not-detected-lab-snomed",
            NOT_TRIGGERED), // Phase 1: excluded by negative-value / timebox / refuted-status filter

        // 4. NEGATIVE LAB (TEXT) — Chlamydia test with valueString "Negative" (uncoded).
        //    Pre-Phase I: triggers.
        //    Post-Phase I: should be NOT_TRIGGERED (uncoded + negative text).
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-negative-lab-text",
            NOT_TRIGGERED), // Phase 1: excluded by negative-value / timebox / refuted-status filter

        // 5. MIXED RESULTS CONTROL — One negative + one positive result on same patient.
        //    Should ALWAYS trigger. Exclusion only applies when ALL entries are negative.
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-mixed-results-control",
            REPORTED),

        // 6. DIAGNOSIS TIMEBOX EXCEEDED — Diagnosis onset 45 days before encounter.
        //    Pre-Phase I: triggers (code matches dxtc).
        //    Post-Phase I: should be NOT_TRIGGERED (exceeds 30-day timebox).
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-dx-timebox-exceeded",
            NOT_TRIGGERED), // Phase 1: excluded by negative-value / timebox / refuted-status filter

        // 7. LAB RESULT TIMEBOX EXCEEDED — Lab result 45 days before encounter start.
        //    Pre-Phase I: triggers.
        //    Post-Phase I: should be NOT_TRIGGERED (exceeds 30-day lab timebox).
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-lab-timebox-exceeded",
            NOT_TRIGGERED), // Phase 1: excluded by negative-value / timebox / refuted-status filter

        // 8. REFUTED DIAGNOSIS — Condition with verificationStatus=refuted.
        //    Pre-Phase I: triggers (code matches dxtc).
        //    Post-Phase I: should be NOT_TRIGGERED (refuted dx excluded).
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-refuted-diagnosis",
            NOT_TRIGGERED), // Phase 1: excluded by negative-value / timebox / refuted-status filter

        // 9. NEG-EXEMPT CONDITION — Gonorrhea with negative result.
        //    RCKMS guidance: Gonorrhea and Hep C remain reportable on negative results
        //    (screening / surveillance value). Phase 1 does not implement this carve-out
        //    and excludes uniformly via the negative-value filter — deferred to Phase 2.
        //    See docs/phase1-fhirpath-workarounds.md §"Deferred: neg-exempt conditions".
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-neg-exempt-condition",
            NOT_TRIGGERED), // Phase 1 design gap; correct behavior requires Phase 2 work

        // ===== BOUNDARY / EDGE CASES =====

        // 10. TIMEBOX 29 DAYS — Lab result 29 days before encounter (within 30d window).
        //     Should trigger both pre- and post-Phase I.
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-edge-timebox-29d",
            REPORTED),

        // 11. TIMEBOX 30 DAYS — Lab result exactly 30 days before encounter (at boundary).
        //     Should trigger both pre- and post-Phase I (boundary inclusive).
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-edge-timebox-30d",
            REPORTED),

        // 12. TIMEBOX 31 DAYS — Lab result 31 days before encounter (outside window).
        //     Pre-Phase I: triggers. Post-Phase I: should be NOT_TRIGGERED.
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-edge-timebox-31d",
            NOT_TRIGGERED), // Phase 1: excluded by negative-value / timebox / refuted-status filter

        // 13. NO EFFECTIVE DATE — Lab result with no effectiveDateTime.
        //     29.8% of real data per RCKMS investigation. Must NOT be excluded.
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-edge-no-effective-date",
            REPORTED),

        // 14. EFFECTIVE PERIOD — Lab result with effectivePeriod (not dateTime), within window.
        //     Should trigger both pre- and post-Phase I.
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-edge-effective-period",
            REPORTED),

        // 15. MIXED CASE TEXT — valueString "Not Detected" (mixed case).
        //     Pre-Phase I: triggers. Post-Phase I: should be NOT_TRIGGERED.
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-edge-mixed-case-text",
            NOT_TRIGGERED), // Phase 1: excluded by negative-value / timebox / refuted-status filter

        // 16. NO VERIFICATION STATUS — Condition without verificationStatus element.
        //     Absence of verificationStatus must NOT cause exclusion.
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-edge-no-verification-status",
            REPORTED),

        // 17. ONSET AS PERIOD — Condition with onsetPeriod instead of onsetDateTime.
        //     Within timebox window. Should trigger both pre- and post-Phase I.
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-edge-onset-period",
            REPORTED),

        // ===== MULTI-TRIGGER COMBOS =====

        // 18. NEG LAB + MEDICATION — Negative lab but also a medication trigger.
        //     Should ALWAYS trigger (medication triggers are not filtered in Phase I).
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-combo-neg-lab-plus-med",
            REPORTED),

        // 19. NEG LAB + REFUTED DX — Both triggers excluded.
        //     Pre-Phase I: triggers. Post-Phase I: should be NOT_TRIGGERED.
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-combo-neg-lab-plus-refuted-dx",
            NOT_TRIGGERED), // Phase 1: excluded by negative-value / timebox / refuted-status filter

        // 20. OLD DX + RECENT LAB ORDER — Old dx timeboxed out, but recent lab order.
        //     Should ALWAYS trigger (lab orders are not filtered in Phase I).
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-combo-old-dx-plus-recent-lab-order",
            REPORTED),

        // 21. NEG LAB + POS LAB + REFUTED DX — Positive lab survives filtering.
        //     Should ALWAYS trigger (one positive result is enough).
        new TestCaseInfo(
            PLAN_DEF_FOLDER,
            PLAN_DEF_URL,
            "phase1-combo-neg-pos-lab-plus-refuted-dx",
            REPORTED));
  }
}
