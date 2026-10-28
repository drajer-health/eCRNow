package com.drajer.bsa.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.BsaTypes.MessageType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.test.util.TestUtils;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.MessageHeader;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ValueSet;
import org.junit.Before;
import org.junit.Test;

/**
 * Filtering, value-set matching and CDA detection in BsaServiceUtils, run on real fixtures: a
 * patient's Conditions (COVID-19, SNOMED 840539006, and pregnancy, SNOMED 77386006), a real
 * Encounter and the eRSD diagnosis trigger value set. Separate from BsaServiceUtilsTest, whose
 * assertions depend on how many tests run before them.
 */
public class BsaServiceUtilsFilterTest {

  private static final String SNOMED = "http://snomed.info/sct";
  private static final String DXTC_URL = "http://hl7.org/fhir/us/ecr/ValueSet/dxtc";

  private final KarProcessingData kd = new KarProcessingData();
  private final Set<Resource> conditions = new HashSet<>();
  private Set<Resource> encounter;

  @Before
  public void loadFixtures() {
    for (BundleEntryComponent entry :
        TestUtils.loadBundleFromFile("R4/Condition/CondPergnancy_SCT_77386006.json").getEntry()) {
      if (entry.getResource() instanceof Condition) {
        conditions.add(entry.getResource());
      }
    }
    encounter =
        Set.of(
            TestUtils.loadResourceDataFromFile(
                Encounter.class, "R4/Encounter/Encounter_97953900.json"));
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.addDependentValueSet(
        TestUtils.loadResourceDataFromFile(
            ValueSet.class,
            "Bsa/Scenarios/PlanDefinition_eRSD_Instance_FhirPathOnly/ex-trigger-condition-in/"
                + "valueset-dxtc-example.json"));
    kd.setKar(kar);
  }

  @Test
  public void filterResources_byValueSetAndByCode_keepsOnlyTheMatchingConditions() {
    DataRequirement byValueSet = new DataRequirement();
    byValueSet.addCodeFilter().setPath("code").setValueSet(DXTC_URL);
    DataRequirement byCode = new DataRequirement();
    byCode.addCodeFilter().setPath("code").addCode(new Coding(SNOMED, "77386006", null));

    assertEquals(
        Set.of("d2572357187", "d2572357189"),
        BsaServiceUtils.filterResources(conditions, byValueSet, kd).stream()
            .map(r -> r.getIdElement().getIdPart())
            .collect(Collectors.toSet()));
    assertEquals(
        Set.of("p73079845"),
        BsaServiceUtils.filterResources(conditions, byCode, kd).stream()
            .map(r -> r.getIdElement().getIdPart())
            .collect(Collectors.toSet()));
  }

  @Test
  public void filterResources_byProfile_keepsOnlyConditionsDeclaringIt() {
    String profile = "http://hl7.org/fhir/us/core/StructureDefinition/us-core-condition";
    Resource declaring =
        conditions.stream()
            .filter(c -> c.getIdElement().getIdPart().equals("d2572357189"))
            .findFirst()
            .orElseThrow();
    declaring.getMeta().addProfile(profile);
    DataRequirement requirement = new DataRequirement();
    requirement.addProfile(profile);

    assertEquals(Set.of(declaring), BsaServiceUtils.filterResources(conditions, requirement, kd));
  }

  /** matchesDateFilter currently always returns true, so date filters keep every resource. */
  @Test
  public void filterByDateFilters_currentlyKeepsEveryCondition() {
    DataRequirement.DataRequirementDateFilterComponent recordedOn20200428 =
        new DataRequirement().addDateFilter().setPath("recordedDate");
    recordedOn20200428.setValue(new DateTimeType("2020-04-28"));
    DataRequirement.DataRequirementDateFilterComponent hasRecordedDate =
        new DataRequirement().addDateFilter().setPath("recordedDate");
    DataRequirement.DataRequirementDateFilterComponent hasAbatement =
        new DataRequirement().addDateFilter().setPath("abatementDateTime");

    assertEquals(
        conditions,
        BsaServiceUtils.filterByDateFilters(conditions, List.of(recordedOn20200428), kd));
    assertEquals(
        conditions, BsaServiceUtils.filterByDateFilters(conditions, List.of(hasRecordedDate), kd));
    assertEquals(
        conditions, BsaServiceUtils.filterByDateFilters(conditions, List.of(hasAbatement), kd));
    assertEquals(
        "no date filters at all keeps everything",
        conditions,
        BsaServiceUtils.filterByDateFilters(conditions, List.of(), kd));
  }

  /** matchesDateFilter currently always returns true, so a period filter keeps the encounter. */
  @Test
  public void filterByDateFilters_currentlyKeepsTheEncounterForAnyPeriod() {
    Date may1 = new DateTimeType("2020-05-01").getValue();
    Date june1 = new DateTimeType("2020-06-01").getValue();
    DataRequirement.DataRequirementDateFilterComponent startedAfterMay =
        new DataRequirement().addDateFilter().setPath("period");
    startedAfterMay.setValue(new Period().setStart(may1));
    DataRequirement.DataRequirementDateFilterComponent startedAfterJune =
        new DataRequirement().addDateFilter().setPath("period");
    startedAfterJune.setValue(new Period().setStart(june1));
    DataRequirement.DataRequirementDateFilterComponent endedByJune =
        new DataRequirement().addDateFilter().setPath("period");
    endedByJune.setValue(new Period().setEnd(june1));

    assertEquals(
        encounter, BsaServiceUtils.filterByDateFilters(encounter, List.of(startedAfterMay), kd));
    assertEquals(
        encounter, BsaServiceUtils.filterByDateFilters(encounter, List.of(startedAfterJune), kd));
    assertEquals(
        encounter, BsaServiceUtils.filterByDateFilters(encounter, List.of(endedByJune), kd));
  }

  @Test
  public void isCodePresentInValueSet_readsComposedConceptsAsWellAsExpansions() {
    ValueSet composed = new ValueSet();
    composed.getCompose().addInclude().setSystem(SNOMED).addConcept().setCode("840539006");

    assertTrue(BsaServiceUtils.isCodePresentInValueSet(composed, SNOMED, "840539006"));
    assertFalse(BsaServiceUtils.isCodePresentInValueSet(composed, SNOMED, "77386006"));
    assertFalse(BsaServiceUtils.isCodePresentInValueSet(composed, "http://loinc.org", "840539006"));
    // isCodePresentInValueSet does not null-check vs before vs.hasExpansion(), so null throws.
    assertThrows(
        NullPointerException.class,
        () -> BsaServiceUtils.isCodePresentInValueSet(null, SNOMED, "840539006"));
    assertEquals(
        "840539006",
        BsaServiceUtils.isCodingPresentInValueSet(composed, new Coding(SNOMED, "840539006", null))
            .getValue1()
            .getValue1());
    assertNull(BsaServiceUtils.isCodingPresentInValueSet(composed, new Coding()));
  }

  @Test
  public void hasCdaData_isTrueOnlyForAMessageBundleLedByACdaEicrHeader() {
    Bundle message = TestUtils.loadBundleFromFile("Bsa/report/ecr-report/fhirReport.json");
    MessageHeader header = (MessageHeader) message.getEntryFirstRep().getResource();
    assertFalse("a FHIR eICR message carries no CDA", BsaServiceUtils.hasCdaData(message));

    header.setEvent(
        new Coding().setCode(BsaTypes.getMessageTypeString(MessageType.CDA_EICR_MESSAGE)));
    assertTrue(BsaServiceUtils.hasCdaData(message));

    message.getEntry().remove(1);
    assertFalse("a header alone is not enough", BsaServiceUtils.hasCdaData(message));
  }
}
