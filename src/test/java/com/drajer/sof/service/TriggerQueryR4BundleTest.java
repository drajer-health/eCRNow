package com.drajer.sof.service;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.sof.utils.R4ResourcesData;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.io.IOUtils;
import org.hl7.fhir.r4.model.*;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class TriggerQueryR4BundleTest {

  @InjectMocks private TriggerQueryR4Bundle triggerQueryR4Bundle;

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
      if (is == null) {
        throw new IllegalArgumentException("Missing test resource file: " + resourcePath);
      }
      byte[] bytes = IOUtils.toByteArray(is);
      String json = new String(bytes, StandardCharsets.UTF_8);
      return fhirContext.newJsonParser().parseResource(type, json);
    }
  }

  @Test
  public void testCreateR4Bundle_withRequiredResourcesOnly() throws Exception {

    Patient patient = loadResourceFromFile("Patient/Patient.json", Patient.class);
    Encounter encounter = loadResourceFromFile("Encounter/enc.json", Encounter.class);
    Condition condition = loadResourceFromFile("Condition/Condition.json", Condition.class);
    Observation observation =
        loadResourceFromFile("Observation/ObservationResource.json", Observation.class);
    ServiceRequest serviceRequest =
        loadResourceFromFile("ServiceRequest/ServiceRequest.json", ServiceRequest.class);

    Bundle mockBundle = new Bundle();
    mockBundle.addEntry(new Bundle.BundleEntryComponent().setResource(patient));
    mockBundle.addEntry(new Bundle.BundleEntryComponent().setResource(encounter));
    mockBundle.addEntry(new Bundle.BundleEntryComponent().setResource(condition));
    mockBundle.addEntry(new Bundle.BundleEntryComponent().setResource(observation));
    mockBundle.addEntry(new Bundle.BundleEntryComponent().setResource(serviceRequest));

    when(r4ResourcesData.getCommonResources(
            eq(r4FhirData), any(), any(), eq(launchDetails), eq(client), eq(fhirContext)))
        .thenReturn(mockBundle);

    Bundle resultBundle =
        triggerQueryR4Bundle.createR4Bundle(launchDetails, r4FhirData, new Date(), new Date());

    assertNotNull("Bundle should not be null", resultBundle);
    assertTrue("Bundle should have entries", resultBundle.hasEntry());

    List<Resource> resources =
        resultBundle.getEntry().stream()
            .map(Bundle.BundleEntryComponent::getResource)
            .collect(Collectors.toList());

    assertTrue(resources.stream().anyMatch(Patient.class::isInstance));
    assertTrue(resources.stream().anyMatch(Encounter.class::isInstance));
    assertTrue(resources.stream().anyMatch(Condition.class::isInstance));
    assertTrue(resources.stream().anyMatch(Observation.class::isInstance));
    assertTrue(resources.stream().anyMatch(ServiceRequest.class::isInstance));

    verify(r4ResourcesData)
        .getCommonResources(
            eq(r4FhirData), any(), any(), eq(launchDetails), eq(client), eq(fhirContext));
  }
}
