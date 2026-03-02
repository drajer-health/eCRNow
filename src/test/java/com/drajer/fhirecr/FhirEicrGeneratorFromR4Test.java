package com.drajer.fhirecr;

import static org.junit.jupiter.api.Assertions.*;

import com.drajer.sof.model.R4FhirData;
import java.util.Date;
import org.hl7.fhir.r4.model.*;
import org.junit.Test;

public class FhirEicrGeneratorFromR4Test {
  @Test
  public void convertR4FhirBundletoCdaEicrtest_Patient_Instance() {
    Patient patient = new Patient();
    patient.setId("patient-1");
    patient.setActive(true);
    patient.setGender(Enumerations.AdministrativeGender.MALE);
    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(patient);
    R4FhirData data = new R4FhirData();
    data.setData(bundle);

    String result = FhirEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(data);
    assertNotNull(result);
    assertEquals("patient-1", patient.getId());
    assertEquals(true, patient.getActive());
  }

  @Test
  public void convertR4FhirBundletoCdaEicrtest_Practitioner_Instance() {
    Practitioner practitioner = new Practitioner();
    practitioner.setId("pract-1");
    Date expected = new Date();
    practitioner.setBirthDate(expected);
    practitioner.setActive(false);
    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(practitioner);
    R4FhirData data = new R4FhirData();
    data.setData(bundle);

    String result = FhirEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(data);
    assertNotNull(result);
    assertEquals("pract-1", practitioner.getId());
    assertEquals(false, practitioner.getActive());
    assertEquals(expected, practitioner.getBirthDate());
  }

  @Test
  public void convertR4FhirBundletoCdaEicrtest_Encounter_Instance() {
    Encounter encounter = new Encounter();
    encounter.setId("encounter-1");

    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(encounter);

    R4FhirData data = new R4FhirData();
    data.setData(bundle);

    String result = FhirEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(data);
    assertNotNull(result);
    assertEquals("encounter-1", data.getEncounter().getId());
  }

  @Test
  public void convertR4FhirBundletoCdaEicrtest_Location_Instance() {
    Location location = new Location();
    location.setId("location-1");

    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(location);

    R4FhirData data = new R4FhirData();
    data.setData(bundle);

    String result = FhirEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(data);
    assertNotNull(result);
    assertEquals("location-1", data.getLocation().getId());
  }

  @Test
  public void convertR4FhirBundletoCdaEicrtest_Condition_Instance() {
    Condition condition = new Condition();
    condition.setId("cond-1");

    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(condition);

    R4FhirData data = new R4FhirData();
    data.setData(bundle);

    String result = FhirEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(data);
    assertNotNull(result);
    assertEquals(1, data.getConditions().size());
    assertEquals("cond-1", data.getConditions().get(0).getId());
  }

  @Test
  public void convertR4FhirBundletoCdaEicrtest_Organization_Instance() {
    Organization org = new Organization();
    org.setId("org-1");

    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(org);

    R4FhirData data = new R4FhirData();
    data.setData(bundle);

    String result = FhirEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(data);
    assertNotNull(result);
    assertEquals("org-1", data.getOrganization().getId());
  }

  @Test
  public void convertR4FhirBundletoCdaEicrtest_DiagnosticReport_Instance() {
    DiagnosticReport report = new DiagnosticReport();
    report.setId("diag-1");

    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(report);

    R4FhirData data = new R4FhirData();
    data.setData(bundle);

    String result = FhirEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(data);
    assertNotNull(result);
    assertEquals(1, data.getDiagReports().size());
    assertEquals("diag-1", data.getDiagReports().get(0).getId());
  }

  @Test
  public void convertR4FhirBundletoCdaEicrtest_MedicationStatement_Instance() {
    MedicationStatement med = new MedicationStatement();
    med.setId("med-1");

    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(med);

    R4FhirData data = new R4FhirData();
    data.setData(bundle);

    String result = FhirEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(data);
    assertNotNull(result);
    assertEquals(1, data.getMedicationStatements().size());
    assertEquals("med-1", data.getMedicationStatements().get(0).getId());
  }

  @Test
  public void convertR4FhirBundletoCdaEicrtest_Immunization_Instance() {
    Immunization immunization = new Immunization();
    immunization.setId("imm-1");

    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(immunization);

    R4FhirData data = new R4FhirData();
    data.setData(bundle);

    String result = FhirEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(data);
    assertNotNull(result);
    assertEquals(1, data.getImmunizations().size());
    assertEquals("imm-1", data.getImmunizations().get(0).getId());
  }

  @Test
  public void convertR4FhirBundletoCdaEicrtest_Observation_Instance() {
    Observation observation = new Observation();
    observation.setId("obs-1");

    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(observation);

    R4FhirData data = new R4FhirData();
    data.setData(bundle);

    String result = FhirEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(data);
    assertNotNull(result);
    assertEquals("obs-1", observation.getId());
  }

  @Test
  public void convertR4FhirBundletoCdaEicrtest_No_Instance() {
    R4FhirData r4FhirData = new R4FhirData();
    String result = FhirEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(r4FhirData);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }
}
