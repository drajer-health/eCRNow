package com.drajer.bsa.utils;

import static org.junit.Assert.*;

import ca.uhn.fhir.context.FhirContext;
import java.util.List;
import org.hl7.fhir.instance.model.api.IBase;
import org.hl7.fhir.r4.hapi.fluentpath.FhirPathR4;
import org.hl7.fhir.r4.model.*;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Unit tests for Phase I FHIRPath expression fragments.
 *
 * <p>Tests the individual FHIRPath expressions used in the updated PlanDefinition condition logic
 * for negative lab value exclusion, timeboxing, and diagnosis verification status filtering. These
 * tests validate expression correctness in isolation without requiring the full eCRNow pipeline.
 */
public class Phase1FhirPathExpressionTest {

  private static FhirContext ctx;
  private static FhirPathR4 fp;

  @BeforeClass
  public static void setup() {
    ctx = FhirContext.forR4();
    fp = new FhirPathR4(ctx);
  }

  private boolean evalBool(Resource resource, String expression) {
    List<IBase> results = fp.evaluate(resource, expression, IBase.class);
    if (results.isEmpty()) return false;
    IBase first = results.get(0);
    if (first instanceof BooleanType) {
      return ((BooleanType) first).booleanValue();
    }
    // Non-empty result list with non-boolean = truthy
    return true;
  }

  // ===== NEGATIVE LAB VALUE EXCLUSION (SNOMED codes) =====

  // Expression fragment: exclude if value contains SNOMED 260385009 or 260415000
  private static final String NEG_VALUE_EXCLUSION =
      "value.ofType(CodeableConcept).coding.where("
          + "code = '260385009' or code = '260415000'"
          + ").exists().not()";

  @Test
  public void testNegativeSnomed260385009_excluded() {
    Observation obs = makeObservation();
    obs.setValue(
        new CodeableConcept()
            .addCoding(new Coding("http://snomed.info/sct", "260385009", "Negative")));
    assertFalse("SNOMED 260385009 should be excluded", evalBool(obs, NEG_VALUE_EXCLUSION));
  }

  @Test
  public void testNotDetectedSnomed260415000_excluded() {
    Observation obs = makeObservation();
    obs.setValue(
        new CodeableConcept()
            .addCoding(new Coding("http://snomed.info/sct", "260415000", "Not detected")));
    assertFalse("SNOMED 260415000 should be excluded", evalBool(obs, NEG_VALUE_EXCLUSION));
  }

  @Test
  public void testPositiveSnomed_notExcluded() {
    Observation obs = makeObservation();
    obs.setValue(
        new CodeableConcept()
            .addCoding(new Coding("http://snomed.info/sct", "10828004", "Positive")));
    assertTrue("Positive SNOMED should NOT be excluded", evalBool(obs, NEG_VALUE_EXCLUSION));
  }

  @Test
  public void testNoValue_notExcluded() {
    Observation obs = makeObservation();
    // No value set at all
    assertTrue(
        "Missing value should NOT be excluded", evalBool(obs, NEG_VALUE_EXCLUSION));
  }

  @Test
  public void testQuantityValue_notExcluded() {
    Observation obs = makeObservation();
    obs.setValue(new Quantity().setValue(42).setUnit("mg/dL"));
    assertTrue(
        "Quantity value should NOT be excluded (no CodeableConcept)",
        evalBool(obs, NEG_VALUE_EXCLUSION));
  }

  // ===== NEGATIVE LAB VALUE EXCLUSION (text values) =====

  private static final String NEG_TEXT_EXCLUSION =
      "value.ofType(string).exists().not() or ("
          + "value.ofType(string).lower().contains('negative').not() "
          + "and value.ofType(string).lower().contains('not detected').not()"
          + ")";

  @Test
  public void testNegativeText_excluded() {
    Observation obs = makeObservation();
    obs.setValue(new StringType("Negative"));
    assertFalse("Text 'Negative' should be excluded", evalBool(obs, NEG_TEXT_EXCLUSION));
  }

  @Test
  public void testNegativeTextUpperCase_excluded() {
    Observation obs = makeObservation();
    obs.setValue(new StringType("NEGATIVE"));
    assertFalse("Text 'NEGATIVE' should be excluded", evalBool(obs, NEG_TEXT_EXCLUSION));
  }

  @Test
  public void testNotDetectedTextMixedCase_excluded() {
    Observation obs = makeObservation();
    obs.setValue(new StringType("Not Detected"));
    assertFalse("Text 'Not Detected' should be excluded", evalBool(obs, NEG_TEXT_EXCLUSION));
  }

  @Test
  public void testNotDetectedTextAllCaps_excluded() {
    Observation obs = makeObservation();
    obs.setValue(new StringType("NOT DETECTED"));
    assertFalse("Text 'NOT DETECTED' should be excluded", evalBool(obs, NEG_TEXT_EXCLUSION));
  }

  @Test
  public void testPositiveText_notExcluded() {
    Observation obs = makeObservation();
    obs.setValue(new StringType("Positive"));
    assertTrue("Text 'Positive' should NOT be excluded", evalBool(obs, NEG_TEXT_EXCLUSION));
  }

  @Test
  public void testNoStringValue_notExcluded() {
    Observation obs = makeObservation();
    // No value at all — guard clause should pass
    assertTrue("No string value should NOT be excluded", evalBool(obs, NEG_TEXT_EXCLUSION));
  }

  @Test
  public void testCodeableConceptValue_notExcludedByTextRule() {
    Observation obs = makeObservation();
    obs.setValue(
        new CodeableConcept()
            .addCoding(new Coding("http://snomed.info/sct", "10828004", "Positive")));
    // CodeableConcept is not a string, so text rule should not exclude
    assertTrue(
        "CodeableConcept value should NOT be excluded by text rule",
        evalBool(obs, NEG_TEXT_EXCLUSION));
  }

  // ===== INTERPRETATION EXCLUSION =====

  private static final String INTERP_EXCLUSION =
      "interpretation.coding.where("
          + "code = '260385009' or code = '260415000'"
          + ").exists().not()";

  @Test
  public void testNegativeInterpretation_excluded() {
    Observation obs = makeObservation();
    obs.addInterpretation(
        new CodeableConcept()
            .addCoding(new Coding("http://snomed.info/sct", "260385009", "Negative")));
    assertFalse(
        "Negative interpretation SNOMED should be excluded", evalBool(obs, INTERP_EXCLUSION));
  }

  @Test
  public void testNoInterpretation_notExcluded() {
    Observation obs = makeObservation();
    assertTrue("No interpretation should NOT be excluded", evalBool(obs, INTERP_EXCLUSION));
  }

  // ===== DIAGNOSIS VERIFICATION STATUS =====

  private static final String VERIFICATION_EXCLUSION =
      "verificationStatus.coding.where("
          + "code in ('entered-in-error' | 'refuted')"
          + ").exists().not()";

  @Test
  public void testRefutedCondition_excluded() {
    Condition cond = makeCondition("refuted");
    assertFalse("Refuted condition should be excluded", evalBool(cond, VERIFICATION_EXCLUSION));
  }

  @Test
  public void testEnteredInErrorCondition_excluded() {
    Condition cond = makeCondition("entered-in-error");
    assertFalse(
        "Entered-in-error condition should be excluded",
        evalBool(cond, VERIFICATION_EXCLUSION));
  }

  @Test
  public void testConfirmedCondition_notExcluded() {
    Condition cond = makeCondition("confirmed");
    assertTrue(
        "Confirmed condition should NOT be excluded", evalBool(cond, VERIFICATION_EXCLUSION));
  }

  @Test
  public void testProvisionalCondition_notExcluded() {
    Condition cond = makeCondition("provisional");
    assertTrue(
        "Provisional condition should NOT be excluded", evalBool(cond, VERIFICATION_EXCLUSION));
  }

  @Test
  public void testNoVerificationStatus_notExcluded() {
    Condition cond = new Condition();
    cond.setCode(
        new CodeableConcept()
            .addCoding(new Coding("http://snomed.info/sct", "840539006", "COVID-19")));
    // No verificationStatus set
    assertTrue(
        "Missing verificationStatus should NOT be excluded",
        evalBool(cond, VERIFICATION_EXCLUSION));
  }

  // ===== TIMEBOX DATE COMPARISONS =====
  // These test the date arithmetic pattern used in the PlanDefinition.
  // We simulate by using Observation.effective vs a known date.

  @Test
  public void testEffectiveDateTimeWithinWindow() {
    // Observation from 2026-02-15, encounter starts 2026-03-01 → 14 days apart → within 30d
    Observation obs = makeObservation();
    obs.setEffective(new DateTimeType("2026-02-15T10:00:00-05:00"));
    String expr = "effective.ofType(dateTime) >= @2026-01-30";
    assertTrue("14 days before should be within 30d window", evalBool(obs, expr));
  }

  @Test
  public void testEffectiveDateTimeOutsideWindow() {
    // Observation from 2026-01-15, encounter 2026-03-01 → 45 days → outside 30d
    Observation obs = makeObservation();
    obs.setEffective(new DateTimeType("2026-01-15T10:00:00-05:00"));
    String expr = "effective.ofType(dateTime) >= @2026-01-30";
    assertFalse("45 days before should be outside 30d window", evalBool(obs, expr));
  }

  @Test
  public void testEffectiveDateTimeExactlyAtBoundary() {
    Observation obs = makeObservation();
    obs.setEffective(new DateTimeType("2026-01-30T08:00:00-05:00"));
    String expr = "effective.ofType(dateTime) >= @2026-01-30";
    assertTrue("Exactly at 30d boundary should be within window", evalBool(obs, expr));
  }

  @Test
  public void testMissingEffectiveDateTime_guardWorks() {
    // No effective date — guard clause should keep it (not exclude)
    Observation obs = makeObservation();
    String expr = "effective.ofType(dateTime).exists().not() or effective.ofType(dateTime) >= @2026-01-30";
    assertTrue("Missing effective should NOT be excluded (guard clause)", evalBool(obs, expr));
  }

  @Test
  public void testEffectivePeriodWithinWindow() {
    Observation obs = makeObservation();
    Period period = new Period();
    period.setStartElement(new DateTimeType("2026-02-25T08:00:00-05:00"));
    period.setEndElement(new DateTimeType("2026-02-25T09:00:00-05:00"));
    obs.setEffective(period);
    String expr = "effective.ofType(Period).start >= @2026-01-30";
    assertTrue("Period.start within window should pass", evalBool(obs, expr));
  }

  // ===== COMBINED EXPRESSION FRAGMENTS =====

  @Test
  public void testCombinedNegativeExclusion_negativeValue() {
    // Full combined check: code match + negative value + within date window
    Observation obs = makeObservation();
    obs.setValue(
        new CodeableConcept()
            .addCoding(new Coding("http://snomed.info/sct", "260385009", "Negative")));
    obs.setEffective(new DateTimeType("2026-03-01T10:00:00-05:00"));

    String combined =
        "(effective.ofType(dateTime).exists().not() "
            + "or effective.ofType(dateTime) >= @2026-01-30) "
            + "and value.ofType(CodeableConcept).coding.where("
            + "code = '260385009' or code = '260415000').exists().not() "
            + "and (value.ofType(string).exists().not() or ("
            + "value.ofType(string).lower().contains('negative').not() "
            + "and value.ofType(string).lower().contains('not detected').not()))";

    assertFalse(
        "Negative SNOMED value within date window should be EXCLUDED", evalBool(obs, combined));
  }

  @Test
  public void testCombinedPositiveValue_notExcluded() {
    Observation obs = makeObservation();
    obs.setValue(
        new CodeableConcept()
            .addCoding(new Coding("http://snomed.info/sct", "10828004", "Positive")));
    obs.setEffective(new DateTimeType("2026-03-01T10:00:00-05:00"));

    String combined =
        "(effective.ofType(dateTime).exists().not() "
            + "or effective.ofType(dateTime) >= @2026-01-30) "
            + "and value.ofType(CodeableConcept).coding.where("
            + "code = '260385009' or code = '260415000').exists().not() "
            + "and (value.ofType(string).exists().not() or ("
            + "value.ofType(string).lower().contains('negative').not() "
            + "and value.ofType(string).lower().contains('not detected').not()))";

    assertTrue(
        "Positive SNOMED value within date window should NOT be excluded",
        evalBool(obs, combined));
  }

  // ===== Helpers =====

  private Observation makeObservation() {
    Observation obs = new Observation();
    obs.setStatus(Observation.ObservationStatus.FINAL);
    obs.setCode(
        new CodeableConcept()
            .addCoding(
                new Coding(
                    "http://loinc.org",
                    "14461-8",
                    "Chlamydia trachomatis [Presence] in Blood")));
    obs.setSubject(new Reference("Patient/test"));
    return obs;
  }

  private Condition makeCondition(String verificationStatus) {
    Condition cond = new Condition();
    cond.setCode(
        new CodeableConcept()
            .addCoding(new Coding("http://snomed.info/sct", "840539006", "COVID-19")));
    cond.setVerificationStatus(
        new CodeableConcept()
            .addCoding(
                new Coding(
                    "http://terminology.hl7.org/CodeSystem/condition-ver-status",
                    verificationStatus,
                    verificationStatus)));
    return cond;
  }
}
