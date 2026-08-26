package com.drajer.bsa.controller;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.parser.IParser;
import com.drajer.bsa.exceptions.InvalidLaunchContext;
import com.drajer.bsa.exceptions.InvalidNotification;
import com.drajer.bsa.model.PatientLaunchContext;
import com.drajer.bsa.service.SubscriptionNotificationReceiver;
import com.drajer.bsa.utils.StartupUtils;
import java.util.Date;
import org.hl7.fhir.r4.model.Bundle;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.util.ReflectionTestUtils;

public class SubscriptionNotificationReceiverControllerTest {

  @InjectMocks
  private SubscriptionNotificationReceiverController subscriptionNotificationReceiverController;

  @Mock private SubscriptionNotificationReceiver subscriptionProcessor;

  @Mock private IParser jsonParser;

  private MockHttpServletRequest request;
  private MockHttpServletResponse response;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    request = new MockHttpServletRequest();
    response = new MockHttpServletResponse();

    ReflectionTestUtils.setField(
        StartupUtils.class, "startTime", new Date(System.currentTimeMillis() - 10000));
    ReflectionTestUtils.setField(StartupUtils.class, "startupTimeDelay", 5);
  }

  @Test
  public void testProcessNotification_Success() throws InvalidLaunchContext, InvalidNotification {
    String notificationBundle = "{ \"resourceType\": \"Bundle\", \"entry\": [] }";
    Bundle bundle = mock(Bundle.class);

    when(jsonParser.parseResource(notificationBundle)).thenReturn(bundle);

    request.addHeader("X-Request-ID", "12345");

    PatientLaunchContext patientLaunchContext = new PatientLaunchContext();
    patientLaunchContext.setPatientId("1234");

    ResponseEntity<Object> responseEntity =
        subscriptionNotificationReceiverController.processNotification(
            notificationBundle, request, response, patientLaunchContext);

    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    assertTrue(responseEntity.getBody().toString().contains("Notification processed successfully"));
  }

  @Test
  public void testProcessNotification_MissingRequestId() throws InvalidLaunchContext {
    String notificationBundle = "{ \"resourceType\": \"Bundle\", \"entry\": [] }";
    request.addHeader("X-Request-ID", "");

    ResponseEntity<Object> responseEntity =
        subscriptionNotificationReceiverController.processNotification(
            notificationBundle, request, response, null);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, responseEntity.getStatusCode());
    assertTrue(
        responseEntity
            .getBody()
            .toString()
            .contains(
                "Unable to process notification since the X-Request-ID HTTP Header is missing"));
  }

  @Test
  public void testProcessNotification_InvalidBundleFormat() throws InvalidLaunchContext {
    String notificationBundle = "{ \"invalid\": \"bundle\" }";

    when(jsonParser.parseResource(notificationBundle)).thenReturn(null);

    request.addHeader("X-Request-ID", "12345");

    PatientLaunchContext patientLaunchContext = new PatientLaunchContext();
    patientLaunchContext.setPatientId("1234");

    ResponseEntity<Object> responseEntity =
        subscriptionNotificationReceiverController.processNotification(
            notificationBundle, request, response, patientLaunchContext);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, responseEntity.getStatusCode());
    assertTrue(
        responseEntity
            .getBody()
            .toString()
            .contains("Unable to parse Resource Param in request body"));
  }

  @Test
  public void testProcessNotification_ProcessingError()
      throws InvalidLaunchContext, InvalidNotification {
    String notificationBundle = "{ \"resourceType\": \"Bundle\", \"entry\": [] }";
    Bundle bundle = mock(Bundle.class);

    when(jsonParser.parseResource(notificationBundle)).thenReturn(bundle);
    doThrow(new RuntimeException("Processing error"))
        .when(subscriptionProcessor)
        .processNotification(any(), any(), any(), any());

    request.addHeader("X-Request-ID", "12345");

    ResponseEntity<Object> responseEntity =
        subscriptionNotificationReceiverController.processNotification(
            notificationBundle, request, response, null);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, responseEntity.getStatusCode());
    assertTrue(
        responseEntity
            .getBody()
            .toString()
            .contains("Unable to process notification, Error: Processing error"));
  }

  @Test
  public void testProcessNotification_InvalidLaunchContext()
      throws InvalidLaunchContext, InvalidNotification {
    String notificationBundle = "{ \"resourceType\": \"Bundle\", \"entry\": [] }";
    Bundle bundle = mock(Bundle.class);

    when(jsonParser.parseResource(notificationBundle)).thenReturn(bundle);
    doThrow(new InvalidLaunchContext("Invalid launch context"))
        .when(subscriptionProcessor)
        .processNotification(any(), any(), any(), any());

    request.addHeader("X-Request-ID", "12345");

    ResponseEntity<Object> responseEntity =
        subscriptionNotificationReceiverController.processNotification(
            notificationBundle, request, response, null);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, responseEntity.getStatusCode());
    assertTrue(
        responseEntity
            .getBody()
            .toString()
            .contains("Unable to process notification, Error: Invalid launch context"));
  }

  @Test
  public void testProcessNotification_DataIntegrityViolationException()
      throws InvalidLaunchContext, InvalidNotification {
    String notificationBundle = "{ \"resourceType\": \"Bundle\", \"entry\": [] }";
    Bundle bundle = mock(Bundle.class);

    when(jsonParser.parseResource(notificationBundle)).thenReturn(bundle);
    doThrow(new DataIntegrityViolationException("Unique Constraint Violation"))
        .when(subscriptionProcessor)
        .processNotification(any(), any(), any(), any());

    request.addHeader("X-Request-ID", "12345");

    ResponseEntity<Object> responseEntity =
        subscriptionNotificationReceiverController.processNotification(
            notificationBundle, request, response, null);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, responseEntity.getStatusCode());
    assertTrue(
        responseEntity
            .getBody()
            .toString()
            .contains("Unable to process notification due to Unique Constraint Violation"));
  }

  @Test
  public void testProcessNotification_AppNotStartedWithDelay() throws InvalidLaunchContext {
    String notificationBundle = "{ \"resourceType\": \"Bundle\", \"entry\": [] }";
    request.addHeader("X-Request-ID", "12345");

    ReflectionTestUtils.setField(
        StartupUtils.class, "startTime", new Date(System.currentTimeMillis() + 10000));

    ResponseEntity<Object> responseEntity =
        subscriptionNotificationReceiverController.processNotification(
            notificationBundle, request, response, null);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, responseEntity.getStatusCode());
    assertTrue(
        responseEntity
            .getBody()
            .toString()
            .contains("Unable to process notification since the app has not started yet"));
  }

  @Test
  public void testProcessNotification_InvalidFHIRBundle() throws InvalidLaunchContext {
    String notificationBundle = "{ \"invalid\": \"bundle\" }";

    when(jsonParser.parseResource(notificationBundle)).thenReturn(null);

    request.addHeader("X-Request-ID", "12345");

    ResponseEntity<Object> responseEntity =
        subscriptionNotificationReceiverController.processNotification(
            notificationBundle, request, response, null);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, responseEntity.getStatusCode());
    assertTrue(
        responseEntity
            .getBody()
            .toString()
            .contains(
                "Unable to parse Resource Param in request body (Has to be a Notification FHIR R4 Bundle)"));
  }

  @Test
  public void testProcessNotification_InvalidFHIRBundleParsing() throws InvalidLaunchContext {
    String notificationBundle = null;

    when(jsonParser.parseResource(notificationBundle)).thenReturn(null);

    request.addHeader("X-Request-ID", "12345");

    ResponseEntity<Object> responseEntity =
        subscriptionNotificationReceiverController.processNotification(
            notificationBundle, request, response, null);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, responseEntity.getStatusCode());

    assertTrue(
        responseEntity
            .getBody()
            .toString()
            .contains(
                "Unable to parse Resource Param in request body (Has to be a Notification FHIR R4 Bundle)"));
  }

  @Test
  public void testProcessNotification_InvalidResourceParsing() throws InvalidLaunchContext {
    String notificationBundle = "{ \"invalid\": \"bundle\" }";
    when(jsonParser.parseResource(notificationBundle)).thenReturn(null);

    request.addHeader("X-Request-ID", "12345");

    ResponseEntity<Object> responseEntity =
        subscriptionNotificationReceiverController.processNotification(
            notificationBundle, request, response, null);

    assertEquals(HttpStatus.UNPROCESSABLE_ENTITY, responseEntity.getStatusCode());

    assertTrue(
        responseEntity
            .getBody()
            .toString()
            .contains(
                "Unable to parse Resource Param in request body (Has to be a Notification FHIR R4 Bundle)"));
  }
}
