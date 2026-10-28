package com.drajer.bsa.kar.action;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import com.drajer.eca.model.MatchedTriggerCodes;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.ResourceType;
import org.javatuples.Pair;
import org.junit.Before;
import org.junit.Test;

/**
 * CheckTriggerCodeStatus records which codes of the eRSD trigger value sets a patient's data
 * matched. The codes are real ones from those value sets: SNOMED 840539006 (disease caused by
 * SARS-CoV-2) and LOINC 94500-6 (SARS-CoV-2 RNA, NAA with probe), under the RCTC value set OID and
 * version used elsewhere in these tests.
 */
public class CheckTriggerCodeStatusTest {

  private static final String SNOMED = "http://snomed.info/sct";
  private static final String LOINC = "http://loinc.org";
  private static final String COVID_CONDITION = SNOMED + "|840539006";
  private static final String COVID_LAB_TEST = LOINC + "|94500-6";
  private static final String VALUE_SET = "RCTC Condition Triggers";
  private static final String VALUE_SET_OID = "2.16.840.1.114222.4.11.7508";
  private static final String VERSION = "20210526";

  private CheckTriggerCodeStatus status;

  @Before
  public void setUp() {
    status = new CheckTriggerCodeStatus();
  }

  @Test
  public void testGetMatchedCode_returnsTheReportableCodeForAMatchingConcept() {
    status.addMatchedTriggerCodes(
        matched("Condition.code", VALUE_SET, VERSION, COVID_CONDITION, COVID_LAB_TEST));

    CodeableConcept concept = new CodeableConcept();
    concept.addCoding().setSystem(SNOMED).setCode("840539006");

    Pair<Boolean, ReportableMatchedTriggerCode> result = status.getMatchedCode(concept);

    assertTrue(result.getValue0());
    ReportableMatchedTriggerCode reportable = result.getValue1();
    assertEquals("840539006", reportable.getCode());
    assertEquals(SNOMED, reportable.getCodeSystem());
    assertEquals(VALUE_SET, reportable.getValueSet());
    assertEquals(VALUE_SET_OID, reportable.getValueSetOid());
    assertEquals(VERSION, reportable.getValueSetVersion());
    assertEquals(Collections.singleton(COVID_CONDITION), reportable.getAllMatches());
  }

  /** A code held only in matchedValues must still be reported when matchedCodes has no hit. */
  @Test
  public void testGetMatchedCode_fallsBackToMatchedValues() {
    MatchedTriggerCodes valuesOnly = matched("Observation.value", VALUE_SET, VERSION);
    valuesOnly.addValue(COVID_LAB_TEST);
    status.addMatchedTriggerCodes(valuesOnly);

    CodeableConcept concept = new CodeableConcept();
    concept.addCoding().setSystem(LOINC).setCode("94500-6");

    Pair<Boolean, ReportableMatchedTriggerCode> result = status.getMatchedCode(concept);

    assertTrue(result.getValue0());
    assertEquals("94500-6", result.getValue1().getCode());
    assertEquals(LOINC, result.getValue1().getCodeSystem());
  }

  @Test
  public void testGetMatchedCode_reportsNothingForAnUnmatchedOrIncompleteConcept() {
    status.addMatchedTriggerCodes(matched("Condition.code", VALUE_SET, VERSION, COVID_CONDITION));

    CodeableConcept otherCode = new CodeableConcept();
    otherCode.addCoding().setSystem(SNOMED).setCode("233604007");
    CodeableConcept noSystem = new CodeableConcept();
    noSystem.addCoding().setCode("840539006");

    for (CodeableConcept concept : Arrays.asList(otherCode, noSystem, new CodeableConcept())) {
      Pair<Boolean, ReportableMatchedTriggerCode> result = status.getMatchedCode(concept);
      assertFalse(result.getValue0());
      assertNull(result.getValue1());
    }
  }

  @Test
  public void testGetMatchedTriggerCodes_findsByPathValueSetAndVersion() {
    MatchedTriggerCodes condition = matched("Condition.code", VALUE_SET, VERSION, COVID_CONDITION);
    status.addMatchedTriggerCodes(condition);

    assertSame(condition, status.getMatchedTriggerCodes("Condition", VALUE_SET, VERSION));
    assertNull(status.getMatchedTriggerCodes("Observation", VALUE_SET, VERSION));
    assertNull(status.getMatchedTriggerCodes("Condition", "Other Value Set", VERSION));
    assertNull(status.getMatchedTriggerCodes("Condition", VALUE_SET, "20990101"));
  }

  @Test
  public void testAddMatchedCodes_createsThenMergesAndFlagsAMatch() {
    assertFalse("nothing matches before any codes are added", status.getTriggerMatchStatus());

    status.addMatchedCodes(
        new HashSet<>(Collections.singleton(COVID_CONDITION)),
        VALUE_SET,
        "Condition.code",
        VERSION);

    assertTrue(status.getTriggerMatchStatus());
    assertEquals(1, status.getMatchedCodes().size());

    status.addMatchedCodes(
        new HashSet<>(Collections.singleton("http://snomed.info/sct|186747009")),
        VALUE_SET,
        "Condition.code",
        VERSION);

    assertEquals(
        "the same path, value set and version must be merged", 1, status.getMatchedCodes().size());
    assertEquals(2, status.getMatchedCodes().get(0).getMatchedCodes().size());

    status.addMatchedCodes(
        new HashSet<>(Collections.singleton(COVID_LAB_TEST)),
        VALUE_SET,
        "Observation.code",
        VERSION);

    assertEquals("a different path is a separate match", 2, status.getMatchedCodes().size());
  }

  @Test
  public void testContainsMatches_looksAtTheMatchedPathOfCodesAndValues() {
    status.addMatchedTriggerCodes(matched("Condition.code", VALUE_SET, VERSION, COVID_CONDITION));
    MatchedTriggerCodes valuesOnly = matched("Observation.value", VALUE_SET, VERSION);
    valuesOnly.addValue(COVID_LAB_TEST);
    status.addMatchedTriggerCodes(valuesOnly);

    assertTrue(status.containsMatches(ResourceType.Condition));
    assertTrue(
        "a match held only in matchedValues counts",
        status.containsMatches(ResourceType.Observation));
    assertFalse(status.containsMatches(ResourceType.Immunization));
  }

  @Test
  public void testAddCopyAndPresenceChecksOnMatchedTriggerCodes() {
    MatchedTriggerCodes condition = matched("Condition.code", VALUE_SET, VERSION, COVID_CONDITION);

    status.addMatchedTriggerCodes(null);
    assertTrue("a null entry must be ignored", status.getMatchedCodes().isEmpty());

    status.addMatchedTriggerCodes(condition);
    CheckTriggerCodeStatus copy = new CheckTriggerCodeStatus();
    copy.copyFrom(status);

    assertEquals(Collections.singletonList(condition), copy.getMatchedCodes());
    assertTrue(copy.isCodePresent(COVID_CONDITION, "Condition.code"));
    assertFalse(copy.isCodePresent(COVID_CONDITION, "Observation.code"));
    assertFalse(copy.isCodePresent(COVID_LAB_TEST, "Condition.code"));

    status.log();
    assertEquals("logging must not alter the recorded matches", 1, status.getMatchedCodes().size());
  }

  private MatchedTriggerCodes matched(
      String path, String valueSet, String version, String... codes) {
    MatchedTriggerCodes matchedTriggerCodes = new MatchedTriggerCodes();
    matchedTriggerCodes.setMatchedPath(path);
    matchedTriggerCodes.setValueSet(valueSet);
    matchedTriggerCodes.setValueSetOid(VALUE_SET_OID);
    matchedTriggerCodes.setValueSetVersion(version);
    Set<String> codeSet = new HashSet<>(Arrays.asList(codes));
    matchedTriggerCodes.setMatchedCodes(codeSet);
    return matchedTriggerCodes;
  }
}
