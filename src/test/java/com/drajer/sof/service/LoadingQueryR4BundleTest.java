package com.drajer.sof.service;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.sof.utils.R4ResourcesData;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;
import org.hl7.fhir.r4.model.*;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class LoadingQueryR4BundleTest {

  @InjectMocks private LoadingQueryR4Bundle loadingQueryR4Bundle;

  @Mock private FhirContextInitializer fhirContextInitializer;

  @Mock private R4ResourcesData r4ResourcesData;

  @Mock private IGenericClient client;

  private FhirContext fhirContext;
  private LaunchDetails launchDetails;
  private R4FhirData r4FhirData;

  @Before
  public void setUp() {
    fhirContext = FhirContext.forR4();

    launchDetails = new LaunchDetails();
    launchDetails.setFhirVersion("R4");
    launchDetails.setLaunchPatientId("test-patient-id");

    r4FhirData = new R4FhirData();

    when(fhirContextInitializer.getFhirContext("R4")).thenReturn(fhirContext);
    when(fhirContextInitializer.createClient(any(), any(), any())).thenReturn(client);
  }

  private <T extends Resource> T loadResourceFromFile(String relativePath, Class<T> type)
      throws Exception {
    String resourcePath = "/R4/" + relativePath;
    try (InputStream is = getClass().getResourceAsStream(resourcePath)) {
      assertNotNull("Missing test resource file: " + resourcePath, is);

      byte[] bytes = org.apache.commons.io.IOUtils.toByteArray(is);
      String json = new String(bytes, StandardCharsets.UTF_8);

      return fhirContext.newJsonParser().parseResource(type, json);
    }
  }

  @Test
  public void testCreateR4Bundle_withAllResources() throws Exception {

    Patient patient = loadResourceFromFile("Patient/Patient.json", Patient.class);
    Encounter encounter = loadResourceFromFile("Encounter/enc.json", Encounter.class);
    Condition condition = loadResourceFromFile("Condition/Condition.json", Condition.class);
    Observation observation =
        loadResourceFromFile("Observation/ObservationResource.json", Observation.class);
    ServiceRequest serviceRequest =
        loadResourceFromFile("ServiceRequest/ServiceRequest.json", ServiceRequest.class);
    Immunization immunization =
        loadResourceFromFile("Immunization/ImmunizationResource.json", Immunization.class);
    DiagnosticReport diagnosticReport =
        loadResourceFromFile("DiagnosticReport/DiagnosticReport.json", DiagnosticReport.class);
    MedicationStatement medStatement =
        loadResourceFromFile(
            "Medication/MedicationStatementResource.json", MedicationStatement.class);
    Bundle commonBundle = new Bundle();
    commonBundle.addEntry(new Bundle.BundleEntryComponent().setResource(patient));
    commonBundle.addEntry(new Bundle.BundleEntryComponent().setResource(encounter));
    commonBundle.addEntry(new Bundle.BundleEntryComponent().setResource(condition));
    commonBundle.addEntry(new Bundle.BundleEntryComponent().setResource(observation));
    commonBundle.addEntry(new Bundle.BundleEntryComponent().setResource(serviceRequest));

    when(r4ResourcesData.getCommonResources(
            eq(r4FhirData), any(), any(), eq(launchDetails), eq(client), eq(fhirContext)))
        .thenReturn(commonBundle);

    when(r4ResourcesData.getResourceFromBundle(any(Bundle.class), eq(Encounter.class)))
        .thenReturn(encounter);

    when(r4ResourcesData.getTravelObservationData(any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(Collections.singletonList(observation));
    when(r4ResourcesData.getSocialHistoryObservationDataOccupation(
            any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(Collections.singletonList(observation));
    when(r4ResourcesData.getPregnancyConditions(any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(Collections.singletonList(condition));
    when(r4ResourcesData.getMedicationStatementData(
            any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(Collections.singletonList(medStatement));
    when(r4ResourcesData.getImmunizationData(any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(Collections.singletonList(immunization));
    when(r4ResourcesData.getDiagnosticReportData(any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(Collections.singletonList(diagnosticReport));

    Bundle resultBundle =
        loadingQueryR4Bundle.createR4Bundle(launchDetails, r4FhirData, new Date(), new Date());

    assertNotNull("Resulting bundle should not be null", resultBundle);
    assertTrue("Bundle should contain at least one entry", resultBundle.hasEntry());

    List<Resource> resources =
        resultBundle.getEntry().stream()
            .map(Bundle.BundleEntryComponent::getResource)
            .collect(Collectors.toList());

    assertTrue(resources.stream().anyMatch(Patient.class::isInstance));
    assertTrue(resources.stream().anyMatch(Encounter.class::isInstance));
    assertTrue(resources.stream().anyMatch(Condition.class::isInstance));
    assertTrue(resources.stream().anyMatch(Observation.class::isInstance));
    assertTrue(resources.stream().anyMatch(ServiceRequest.class::isInstance));
    assertTrue(resources.stream().anyMatch(Immunization.class::isInstance));
    assertTrue(resources.stream().anyMatch(DiagnosticReport.class::isInstance));
    assertTrue(resources.stream().anyMatch(MedicationStatement.class::isInstance));

    assertEquals(1, resources.stream().filter(Patient.class::isInstance).count());
    assertEquals(1, resources.stream().filter(Encounter.class::isInstance).count());

    verify(r4ResourcesData)
        .getCommonResources(
            eq(r4FhirData), any(), any(), eq(launchDetails), eq(client), eq(fhirContext));
    verify(r4ResourcesData).getImmunizationData(any(), any(), any(), any(), any(), any(), any());
    verify(r4ResourcesData)
        .getDiagnosticReportData(any(), any(), any(), any(), any(), any(), any());
    verify(r4ResourcesData)
        .getMedicationStatementData(any(), any(), any(), any(), any(), any(), any());
    verify(r4ResourcesData).getPregnancyConditions(any(), any(), any(), any(), any(), any(), any());
    verify(r4ResourcesData)
        .getTravelObservationData(any(), any(), any(), any(), any(), any(), any());
    verify(r4ResourcesData)
        .getSocialHistoryObservationDataOccupation(any(), any(), any(), any(), any(), any(), any());
  }
}
