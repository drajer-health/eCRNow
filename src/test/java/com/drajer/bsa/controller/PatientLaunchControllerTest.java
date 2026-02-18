package com.drajer.bsa.controller;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.model.*;
import com.drajer.bsa.service.HealthcareSettingsService;
import com.drajer.bsa.service.SubscriptionNotificationReceiver;
import com.drajer.bsa.utils.StartupUtils;
import com.drajer.ecrapp.model.ScheduledTasks;
import com.drajer.ecrapp.service.SchedulerService;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.util.*;
import org.hl7.fhir.r4.model.Encounter;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.*;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@RunWith(PowerMockRunner.class)
@PrepareForTest({StartupUtils.class})
public class PatientLaunchControllerTest {

  private static final String PATIENT_LAUNCH_JSON = "Bsa/patient-launch.json";
  @Mock private HealthcareSettingsService hsService;
  @Mock private EhrQueryService ehrService;
  @Mock private SubscriptionNotificationReceiver notificationReceiver;
  @Mock private SchedulerService schedulerService;
  @Mock private HttpServletRequest request;
  @Mock private HttpServletResponse response;

  @InjectMocks private PatientLaunchController controller;

  private PatientLaunchContext launchContext;
  private HealthcareSetting hs;

  @Before
  public void setup() {

    MockitoAnnotations.initMocks(this);

    // 🔥 Load request payload from JSON file
    launchContext =
        TestUtils.readFileContents(
            PATIENT_LAUNCH_JSON, new TypeReference<PatientLaunchContext>() {});

    hs = new HealthcareSetting();
    hs.setFhirServerBaseURL(launchContext.getFhirServerURL());

    when(request.getHeader("X-Request-ID")).thenReturn("req-123");

    PowerMockito.mockStatic(StartupUtils.class);
    when(StartupUtils.hasAppStarted()).thenReturn(true);
  }

  // =====================================================
  // launchPatient SUCCESS
  // =====================================================
  @Test
  public void testLaunchPatient_success_operationOutcome() {

    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(hs);

    when(ehrService.getResourceById(any(), anyString(), anyString(), anyBoolean()))
        .thenReturn(new Encounter());

    ResponseEntity<Object> result = controller.launchPatient(launchContext, request, response);

    assertEquals(HttpStatus.OK, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("information", outcome.at("/issue/0/severity").asText());

    assertTrue(outcome.at("/issue/0/details/text").asText().contains("Patient Instance launched"));
  }

  @Test
  public void testLaunchPatient_missingHeader_errorOutcome() {

    when(request.getHeader("X-Request-ID")).thenReturn(null);
    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(hs);

    ResponseEntity<Object> result = controller.launchPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(outcome.at("/issue/0/details/text").asText().contains("No Request Id"));
  }

  @Test
  public void testReLaunchPatient_success_operationOutcome() {

    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(hs);

    when(ehrService.getResourceById(any(), anyString(), anyString(), anyBoolean()))
        .thenReturn(new Encounter());

    ResponseEntity<Object> result = controller.reLaunchPatient(launchContext, request, response);

    assertEquals(HttpStatus.OK, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("information", outcome.at("/issue/0/severity").asText());
  }

  @Test
  public void testReProcessPatient_success_operationOutcome() {

    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(hs);

    when(ehrService.getResourceById(any(), anyString(), anyString(), anyBoolean()))
        .thenReturn(new Encounter());

    ResponseEntity<Object> result = controller.reProcessPatient(launchContext, request, response);

    assertEquals(HttpStatus.OK, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("information", outcome.at("/issue/0/severity").asText());
  }

  @Test
  public void testDeleteScheduledTasks_success_operationOutcome() throws Exception {

    when(schedulerService.delete(any(), anyString(), anyString(), anyString()))
        .thenReturn(Arrays.asList(new ScheduledTasks()));

    ResponseEntity<Object> result =
        controller.deleteScheduledTasks(
            launchContext.getFhirServerURL(),
            launchContext.getPatientId(),
            launchContext.getEncounterId());

    assertEquals(HttpStatus.OK, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("information", outcome.at("/issue/0/severity").asText());
  }

  private JsonNode getOutcome(ResponseEntity<Object> response) {
    return (JsonNode) response.getBody();
  }
}
