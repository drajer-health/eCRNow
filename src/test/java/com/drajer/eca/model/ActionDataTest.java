package com.drajer.eca.model;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.Enumerations.FHIRAllTypes;
import org.hl7.fhir.r4.model.TriggerDefinition.TriggerType;
import org.junit.Before;
import org.junit.Test;

public class ActionDataTest {

  private ActionData actionData;

  @Before
  public void setUp() {
    actionData = new ActionData();
  }

  @Test
  public void testSettersAndGetters() {

    actionData.setFhirDataType(FHIRAllTypes.PATIENT);
    actionData.setTriggerType(TriggerType.DATAADDED);
    actionData.setPath("Patient.name");
    actionData.setValueSet(new CanonicalType("http://example.org/fhir/ValueSet/test"));
    actionData.setProfiles(
        Arrays.asList(
            new CanonicalType("http://example.org/fhir/StructureDefinition/profile1"),
            new CanonicalType("http://example.org/fhir/StructureDefinition/profile2")));

    assertEquals(FHIRAllTypes.PATIENT, actionData.getFhirDataType());
    assertEquals(TriggerType.DATAADDED, actionData.getTriggerType());
    assertEquals("Patient.name", actionData.getPath());
    assertEquals("http://example.org/fhir/ValueSet/test", actionData.getValueSet().getValue());
    assertEquals(2, actionData.getProfiles().size());
    assertEquals(
        "http://example.org/fhir/StructureDefinition/profile1",
        actionData.getProfiles().get(0).getValue());
  }

  @Test
  public void testPrintRunsWithoutError() {
    actionData.setFhirDataType(FHIRAllTypes.OBSERVATION);
    actionData.setTriggerType(TriggerType.DATAACCESSED);
    actionData.setPath("Observation.value");
    actionData.setValueSet(new CanonicalType("http://example.org/fhir/ValueSet/obs"));
    actionData.setProfiles(
        Arrays.asList(new CanonicalType("profile1"), new CanonicalType("profile2")));
    assertEquals(FHIRAllTypes.OBSERVATION, actionData.getFhirDataType());
    assertEquals(TriggerType.DATAACCESSED, actionData.getTriggerType());
    assertEquals("Observation.value", actionData.getPath());
    assertEquals("http://example.org/fhir/ValueSet/obs", actionData.getValueSet().getValue());
    assertEquals(2, actionData.getProfiles().size());
    assertEquals("profile1", actionData.getProfiles().get(0).getValue());
    assertEquals("profile2", actionData.getProfiles().get(1).getValue());
    actionData.print();
  }
}
