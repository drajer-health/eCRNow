package com.drajer.bsa.controller;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.rest.server.exceptions.AuthenticationException;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.exceptions.InvalidLaunchContext;
import com.drajer.bsa.exceptions.InvalidNotification;
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
import org.springframework.dao.DataIntegrityViolationException;
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

    launchContext =
        TestUtils.readFileContents(
            PATIENT_LAUNCH_JSON, new TypeReference<PatientLaunchContext>() {});

    hs = new HealthcareSetting();
    hs.setFhirServerBaseURL(launchContext.getFhirServerURL());

    when(request.getHeader("X-Request-ID")).thenReturn("req-123");

    PowerMockito.mockStatic(StartupUtils.class);
    when(StartupUtils.hasAppStarted()).thenReturn(true);
  }

  // ============= launchPatient Tests =============

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
  public void testLaunchPatient_emptyRequestId_errorOutcome() {

    when(request.getHeader("X-Request-ID")).thenReturn("");
    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(hs);

    ResponseEntity<Object> result = controller.launchPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());
  }

  @Test
  public void testLaunchPatient_nullHealthcareSetting_errorOutcome() {

    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(null);

    ResponseEntity<Object> result = controller.launchPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(
        outcome.at("/issue/0/details/text").asText().contains("Unrecognized healthcare setting"));
  }

  @Test
  public void testLaunchPatient_authenticationException() {

    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(hs);

    when(ehrService.getResourceById(any(), anyString(), anyString(), anyBoolean()))
        .thenThrow(new AuthenticationException("Authentication failed"));

    ResponseEntity<Object> result = controller.launchPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNAUTHORIZED, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(outcome.at("/issue/0/details/text").asText().contains("Authentication failed"));
  }

  @Test
  public void testLaunchPatient_dataIntegrityViolationException()
      throws InvalidNotification, InvalidLaunchContext {

    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(hs);

    when(notificationReceiver.processNotification(any(), any(), any(), any()))
        .thenThrow(new DataIntegrityViolationException("Unique Constraint Violation"));

    ResponseEntity<Object> result = controller.launchPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(
        outcome.at("/issue/0/details/text").asText().contains("Unique Constraint Violation"));
  }

  @Test
  public void testLaunchPatient_generalException()
      throws InvalidNotification, InvalidLaunchContext {

    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(hs);

    when(notificationReceiver.processNotification(any(), any(), any(), any()))
        .thenThrow(new RuntimeException("General error occurred"));

    ResponseEntity<Object> result = controller.launchPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(outcome.at("/issue/0/details/text").asText().contains("General error occurred"));
  }

  @Test
  public void testLaunchPatient_appNotStarted() {

    when(StartupUtils.hasAppStarted()).thenReturn(false);
    when(StartupUtils.getPatientLaunchInstanceTime()).thenReturn(new Date());

    ResponseEntity<Object> result = controller.launchPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(outcome.at("/issue/0/details/text").asText().contains("has not started yet"));
  }

  // ============= reLaunchPatient Tests =============

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
  public void testReLaunchPatient_missingRequestId_errorOutcome() {

    when(request.getHeader("X-Request-ID")).thenReturn(null);
    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(hs);

    ResponseEntity<Object> result = controller.reLaunchPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(outcome.at("/issue/0/details/text").asText().contains("No Request Id"));
  }

  @Test
  public void testReLaunchPatient_nullHealthcareSetting_errorOutcome() {

    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(null);

    ResponseEntity<Object> result = controller.reLaunchPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(
        outcome.at("/issue/0/details/text").asText().contains("Unrecognized healthcare setting"));
  }

  @Test
  public void testReLaunchPatient_authenticationException()
      throws InvalidNotification, InvalidLaunchContext {

    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(hs);

    when(notificationReceiver.processRelaunchNotification(any(), any(), any(), any(), anyBoolean()))
        .thenThrow(new AuthenticationException("Auth failed on relaunch"));

    ResponseEntity<Object> result = controller.reLaunchPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNAUTHORIZED, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(outcome.at("/issue/0/details/text").asText().contains("Auth failed on relaunch"));
  }

  @Test
  public void testReLaunchPatient_generalException()
      throws InvalidNotification, InvalidLaunchContext {

    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(hs);

    when(notificationReceiver.processRelaunchNotification(any(), any(), any(), any(), anyBoolean()))
        .thenThrow(new RuntimeException("Relaunch error"));

    ResponseEntity<Object> result = controller.reLaunchPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(outcome.at("/issue/0/details/text").asText().contains("Relaunch error"));
  }

  @Test
  public void testReLaunchPatient_appNotStarted() {

    when(StartupUtils.hasAppStarted()).thenReturn(false);
    when(StartupUtils.getPatientLaunchInstanceTime()).thenReturn(new Date());

    ResponseEntity<Object> result = controller.reLaunchPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(outcome.at("/issue/0/details/text").asText().contains("has not started yet"));
  }

  // ============= reProcessPatient Tests =============

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
  public void testReProcessPatient_missingRequestId_errorOutcome() {

    when(request.getHeader("X-Request-ID")).thenReturn(null);
    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(hs);

    ResponseEntity<Object> result = controller.reProcessPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(outcome.at("/issue/0/details/text").asText().contains("No Request Id"));
  }

  @Test
  public void testReProcessPatient_nullHealthcareSetting_errorOutcome() {

    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(null);

    ResponseEntity<Object> result = controller.reProcessPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(
        outcome.at("/issue/0/details/text").asText().contains("Unrecognized healthcare setting"));
  }

  @Test
  public void testReProcessPatient_authenticationException()
      throws InvalidNotification, InvalidLaunchContext {

    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(hs);

    when(notificationReceiver.reProcessNotification(any(), any(), any(), any(), anyBoolean()))
        .thenThrow(new AuthenticationException("Auth failed on reprocess"));

    ResponseEntity<Object> result = controller.reProcessPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNAUTHORIZED, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(outcome.at("/issue/0/details/text").asText().contains("Auth failed on reprocess"));
  }

  @Test
  public void testReProcessPatient_generalException()
      throws InvalidNotification, InvalidLaunchContext {

    when(hsService.getHealthcareSettingByUrl(anyString())).thenReturn(hs);

    when(notificationReceiver.reProcessNotification(any(), any(), any(), any(), anyBoolean()))
        .thenThrow(new RuntimeException("Reprocess error"));

    ResponseEntity<Object> result = controller.reProcessPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(outcome.at("/issue/0/details/text").asText().contains("Reprocess error"));
  }

  @Test
  public void testReProcessPatient_appNotStarted() {

    when(StartupUtils.hasAppStarted()).thenReturn(false);
    when(StartupUtils.getPatientLaunchInstanceTime()).thenReturn(new Date());

    ResponseEntity<Object> result = controller.reProcessPatient(launchContext, request, response);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(outcome.at("/issue/0/details/text").asText().contains("has not started yet"));
  }

  // ============= deleteScheduledTasks Tests =============

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

    assertTrue(outcome.at("/issue/0/details/text").asText().contains("deleted Successfully"));
  }

  @Test
  public void testDeleteScheduledTasks_nullFhirServerUrl_badRequest() throws Exception {

    ResponseEntity<Object> result =
        controller.deleteScheduledTasks(
            null, launchContext.getPatientId(), launchContext.getEncounterId());

    assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(
        outcome
            .at("/issue/0/details/text")
            .asText()
            .contains("PatientId , EncounterId and fhirServerBaseUrl are required"));
  }

  @Test
  public void testDeleteScheduledTasks_emptyFhirServerUrl_badRequest() throws Exception {

    ResponseEntity<Object> result =
        controller.deleteScheduledTasks(
            "", launchContext.getPatientId(), launchContext.getEncounterId());

    assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());
  }

  @Test
  public void testDeleteScheduledTasks_nullPatientId_badRequest() throws Exception {

    ResponseEntity<Object> result =
        controller.deleteScheduledTasks(
            launchContext.getFhirServerURL(), null, launchContext.getEncounterId());

    assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());
  }

  @Test
  public void testDeleteScheduledTasks_emptyPatientId_badRequest() throws Exception {

    ResponseEntity<Object> result =
        controller.deleteScheduledTasks(
            launchContext.getFhirServerURL(), "", launchContext.getEncounterId());

    assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());
  }

  @Test
  public void testDeleteScheduledTasks_nullEncounterId_badRequest() throws Exception {

    ResponseEntity<Object> result =
        controller.deleteScheduledTasks(
            launchContext.getFhirServerURL(), launchContext.getPatientId(), null);

    assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());
  }

  @Test
  public void testDeleteScheduledTasks_emptyEncounterId_badRequest() throws Exception {

    ResponseEntity<Object> result =
        controller.deleteScheduledTasks(
            launchContext.getFhirServerURL(), launchContext.getPatientId(), "");

    assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());
  }

  @Test
  public void testDeleteScheduledTasks_allParametersNull_badRequest() throws Exception {

    ResponseEntity<Object> result = controller.deleteScheduledTasks(null, null, null);

    assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());
  }

  @Test
  public void testDeleteScheduledTasks_noTasksFound_notFound() throws Exception {

    when(schedulerService.delete(any(), anyString(), anyString(), anyString()))
        .thenReturn(new ArrayList<>());

    ResponseEntity<Object> result =
        controller.deleteScheduledTasks(
            launchContext.getFhirServerURL(),
            launchContext.getPatientId(),
            launchContext.getEncounterId());

    assertEquals(HttpStatus.NOT_FOUND, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(
        outcome
            .at("/issue/0/details/text")
            .asText()
            .contains("No Scheduled Tasks found to delete"));
  }

  @Test
  public void testDeleteScheduledTasks_nullTaskList_notFound() throws Exception {

    when(schedulerService.delete(any(), anyString(), anyString(), anyString())).thenReturn(null);

    ResponseEntity<Object> result =
        controller.deleteScheduledTasks(
            launchContext.getFhirServerURL(),
            launchContext.getPatientId(),
            launchContext.getEncounterId());

    assertEquals(HttpStatus.NOT_FOUND, result.getStatusCode());

    JsonNode outcome = getOutcome(result);

    assertEquals("error", outcome.at("/issue/0/severity").asText());

    assertTrue(
        outcome
            .at("/issue/0/details/text")
            .asText()
            .contains("No Scheduled Tasks found to delete"));
  }

  @Test
  public void testDeleteScheduledTasks_multipleTasksFound_success() throws Exception {

    List<ScheduledTasks> tasks = new ArrayList<>();
    tasks.add(new ScheduledTasks());
    tasks.add(new ScheduledTasks());
    tasks.add(new ScheduledTasks());

    when(schedulerService.delete(any(), anyString(), anyString(), anyString())).thenReturn(tasks);

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
