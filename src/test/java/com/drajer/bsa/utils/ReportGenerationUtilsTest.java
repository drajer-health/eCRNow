package com.drajer.bsa.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.fhirecr.FhirGeneratorConstants;
import java.util.HashSet;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.codesystems.ObservationCategory;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class ReportGenerationUtilsTest {

  private static Set<Resource> resources = new HashSet<>();

  @Test
  public void testFilterObservationsByCategory() {
    FhirContext fhirContext = FhirContext.forR4();
    Bundle bundle =
        fhirContext
            .newJsonParser()
            .parseResource(
                Bundle.class,
                ReportGenerationUtilsTest.class.getResourceAsStream(
                    "/R4/Observation/ObservationStart.json"));
    bundle
        .getEntry()
        .forEach(
            (e) -> {
              resources.add(e.getResource());
            });
    Set<Resource> filteredObservation =
        ReportGenerationUtils.filterObservationsByCategory(resources, "social-history");
    assertEquals(6, filteredObservation.size());

    filteredObservation =
        ReportGenerationUtils.filterObservationsByCategory(
            resources, ObservationCategory.LABORATORY.toCode());
    assertEquals(1, filteredObservation.size());
  }

  @Test
  public void testGetVitalSectionObservation() {
    Set<Resource> resources = new HashSet<>();
    FhirContext fhirContext = FhirContext.forR4();
    Bundle bundle =
        fhirContext
            .newJsonParser()
            .parseResource(
                Bundle.class,
                ReportGenerationUtilsTest.class.getResourceAsStream(
                    "/R4/Observation/ObservationEnd.json"));
    bundle
        .getEntry()
        .forEach(
            (e) -> {
              resources.add(e.getResource());
            });
    Set<Resource> filteredObservation = ReportGenerationUtils.getVitalSectionObservation(resources);

    assertEquals(5, filteredObservation.size());
  }

  @Test
  public void testGetAssessmentObservations() {
    Set<Resource> resources = new HashSet<>();
    FhirContext fhirContext = FhirContext.forR4();
    Bundle bundle =
        fhirContext
            .newJsonParser()
            .parseResource(
                Bundle.class,
                ReportGenerationUtilsTest.class.getResourceAsStream(
                    "/R4/Observation/ObservationStart.json"));
    bundle
        .getEntry()
        .forEach(
            (e) -> {
              resources.add(e.getResource());
            });
    Set<Resource> filteredObservation = ReportGenerationUtils.getAssessmentObservations(resources);

    assertEquals(4, filteredObservation.size());
  }

  @Test
  public void testGetNotesDiagnosticReports() {
    Set<Resource> resources = new HashSet<>();
    FhirContext fhirContext = FhirContext.forR4();
    Bundle bundle =
        fhirContext
            .newJsonParser()
            .parseResource(
                Bundle.class,
                ReportGenerationUtilsTest.class.getResourceAsStream(
                    "/R4/DiagnosticReport/DiagnosticReport_2.json"));
    bundle
        .getEntry()
        .forEach(
            (e) -> {
              resources.add(e.getResource());
            });
    Set<Resource> filtered = ReportGenerationUtils.getNotesDiagnosticReports(resources);

    assertEquals(2, filtered.size());
  }

  @Test
  public void testFilterDiagnosticReportsByCategories() {
    Set<Resource> resources = new HashSet<>();
    FhirContext fhirContext = FhirContext.forR4();
    Bundle bundle =
        fhirContext
            .newJsonParser()
            .parseResource(
                Bundle.class,
                ReportGenerationUtilsTest.class.getResourceAsStream(
                    "/R4/DiagnosticReport/DiagnosticReport_2.json"));
    bundle
        .getEntry()
        .forEach(
            (e) -> {
              resources.add(e.getResource());
            });
    Set<Resource> filtered =
        ReportGenerationUtils.filterDiagnosticReportsByCategories(
            resources,
            FhirGeneratorConstants.TERMINOLOGY_V2_0074_URL,
            FhirGeneratorConstants.LAB_CODE);

    assertEquals(1, filtered.size());

    filtered =
        ReportGenerationUtils.filterDiagnosticReportsByCategories(
            resources,
            FhirGeneratorConstants.TERMINOLOGY_V2_0074_URL,
            FhirGeneratorConstants.LAB_CODE,
            "RAD");

    assertEquals(2, filtered.size());
  }
}
