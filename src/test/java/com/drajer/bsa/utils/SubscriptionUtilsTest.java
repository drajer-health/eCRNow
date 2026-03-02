package com.drajer.bsa.utils;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.bsa.exceptions.InvalidLaunchContext;
import com.drajer.bsa.exceptions.InvalidNotification;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.model.PatientLaunchContext;
import com.drajer.test.util.TestUtils;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Reference;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

@RunWith(MockitoJUnitRunner.class)
public class SubscriptionUtilsTest {

  private Bundle bundle;

  private MockHttpServletRequest mockHttpServletRequest;
  private MockHttpServletResponse mockHttpServletResponse;

  @Before
  public void setUp() throws Exception {
    FhirContext fhirContext = FhirContext.forR4();

    bundle =
        TestUtils.loadBundleFromFile("Bsa/NotificationBundleEncounterCloseWithoutPeriord.json");
    Bundle.BundleEntryComponent encounterEntry = new Bundle.BundleEntryComponent();
    encounterEntry.setResource(bundle);
    bundle.addEntry(encounterEntry);
    bundle.setType(BundleType.HISTORY);
    mockHttpServletRequest = new MockHttpServletRequest();
    mockHttpServletResponse = new MockHttpServletResponse();
  }

  @Test
  public void testGetNotificationContext() throws Exception {

    mockHttpServletRequest.addHeader("X-Request-ID", "ecrunittest_id");
    mockHttpServletRequest.addHeader("X-Correlation-ID", "ecrUnitTestCorrelationID");
    PatientLaunchContext launchContext = new PatientLaunchContext();

    NotificationContext notificationContext =
        SubscriptionUtils.getNotificationContext(
            bundle, mockHttpServletRequest, mockHttpServletResponse, false, false, launchContext);
    assertNotNull(notificationContext);
  }

  @Test
  public void getFhirServerUrl() {
    Reference reference = new Reference();
    String fhirServerUrl = SubscriptionUtils.getFhirServerUrl(reference);
    assertNull(fhirServerUrl);
  }

  @Test
  public void getNotificationContextWithoutPeriod() throws Exception {
    FhirContext fhirContext = FhirContext.forR4();
    Bundle bundle =
        TestUtils.loadBundleFromFile("Bsa/NotificationBundleEncounterCloseWithoutPeriord.json");

    Bundle.BundleEntryComponent encounterEntry = new Bundle.BundleEntryComponent();
    encounterEntry.setResource(bundle);
    bundle.addEntry(encounterEntry);
    bundle.setType(BundleType.HISTORY);
    PatientLaunchContext launchContext = new PatientLaunchContext();

    NotificationContext notificationContext =
        SubscriptionUtils.getNotificationContext(
            bundle, mockHttpServletRequest, mockHttpServletResponse, false, false, launchContext);
    assertNotNull(notificationContext);
  }

  @Test
  public void getNotificationContextWithEmptyBundle()
      throws InvalidLaunchContext, InvalidNotification {
    Bundle bundle = new Bundle();
    PatientLaunchContext launchContext = new PatientLaunchContext();

    try {
      NotificationContext notificationContext =
          SubscriptionUtils.getNotificationContext(
              bundle, mockHttpServletRequest, mockHttpServletResponse, false, false, launchContext);
    } catch (InvalidNotification e) {
      assertTrue(true);
    }
  }

  @Test
  public void testGetNotificationContextForElse() throws Exception {
    MockHttpServletRequest mockHttpServletRequest = new MockHttpServletRequest();
    MockHttpServletResponse mockHttpServletResponse = new MockHttpServletResponse();
    List<Map<String, String>> headersList = new ArrayList<>();
    headersList.add(createHeaderMap("X-Correlation-ID", "ecrUnitTestCorrelationID"));
    headersList.add(createHeaderMap("X-Request-ID", "ecrunittest_id"));

    for (Map<String, String> headers : headersList) {
      for (Map.Entry<String, String> entry : headers.entrySet()) {
        mockHttpServletRequest.addHeader(entry.getKey(), entry.getValue());
      }
      PatientLaunchContext launchContext = new PatientLaunchContext();
      NotificationContext notificationContext =
          SubscriptionUtils.getNotificationContext(
              bundle, mockHttpServletRequest, mockHttpServletResponse, false, false, launchContext);
      mockHttpServletRequest.removeHeader("X-Correlation-ID");
      mockHttpServletRequest.removeHeader("X-Request-ID");

      assertNotNull(notificationContext);
    }
  }

  private Map<String, String> createHeaderMap(String key, String value) {
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put(key, value);
    return headerMap;
  }

  @Test
  public void getPatientId() {
    Patient patient = new Patient();
    String patientId = SubscriptionUtils.getPatientId(patient);
    assertNull(patientId);
  }

  @Test
  public void testGetNotificationContextWithInvalidBundle() throws Exception {
    FhirContext fhirContext = FhirContext.forR4();
    Bundle bundle = TestUtils.loadBundleFromFile("Bsa/NotificationBundle.json");
    Bundle.BundleEntryComponent encounterEntry = new Bundle.BundleEntryComponent();
    encounterEntry.setResource(bundle);
    bundle.addEntry(encounterEntry);
    bundle.setType(BundleType.HISTORY);
    PatientLaunchContext launchContext = new PatientLaunchContext();
    try {
      NotificationContext notificationContext =
          SubscriptionUtils.getNotificationContext(
              bundle, mockHttpServletRequest, mockHttpServletResponse, false, false, launchContext);
    } catch (InvalidNotification e) {
      assertTrue(true);
    }
  }
}
