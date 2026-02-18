package com.drajer.bsa.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.bsa.service.RrReceiver;
import com.drajer.ecrapp.model.ReportabilityResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.junit.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.MockitoJUnitRunner;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.server.ResponseStatusException;

@ExtendWith(MockitoExtension.class)
@RunWith(MockitoJUnitRunner.class)
public class ReportabilityResponseControllerTest {

  @InjectMocks private ReportabilityResponseController reportabilityResponseController;

  @Mock private RrReceiver rrReceieverService;
  @Mock private PublicHealthMessagesDao phDao;

  @Mock private HttpServletRequest request;

  @Mock private HttpServletResponse response;

  @Mock private ReportabilityResponse reportabilityResponse;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testReceiveReportabilityResponse_MDN_Valid() {

    String xRequestId = "xRequestId123";
    String xCorrelationId = "xCorrelationId123";
    when(reportabilityResponse.getResponseType())
        .thenReturn(ReportabilityResponse.MDN_RESPONSE_TYPE);
    doNothing()
        .when(rrReceieverService)
        .handleFailureMdn(reportabilityResponse, xCorrelationId, xRequestId);

    ResponseEntity<String> responseEntity =
        reportabilityResponseController.receiveReportabilityResponse(
            xRequestId, xCorrelationId, reportabilityResponse, request, response);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  public void testReceiveReportabilityResponse_RR_Valid() {

    String xRequestId = "xRequestId123";
    String xCorrelationId = "xCorrelationId123";

    when(reportabilityResponse.getResponseType()).thenReturn("RR");

    doNothing()
        .when(rrReceieverService)
        .handleReportabilityResponse(reportabilityResponse, xRequestId);

    ResponseEntity<String> responseEntity =
        reportabilityResponseController.receiveReportabilityResponse(
            xRequestId, xCorrelationId, reportabilityResponse, request, response);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  public void testReceiveReportabilityResponse_BadRequest_InvalidData() {

    String xRequestId = "xRequestId123";
    String xCorrelationId = "xCorrelationId123";
    try {
      when(reportabilityResponse.getResponseType()).thenReturn(null);

      ResponseEntity<String> responseEntity =
          reportabilityResponseController.receiveReportabilityResponse(
              xRequestId, xCorrelationId, reportabilityResponse, request, response);
    } catch (ResponseStatusException e) {
      assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, e.getStatusCode());
    }
  }

  @Test
  public void testReceiveReportabilityResponse_InternalServerError() {

    String xRequestId = "xRequestId123";
    String xCorrelationId = "xCorrelationId123";
    try {
      when(reportabilityResponse.getResponseType()).thenReturn("RR");

      doThrow(new RuntimeException("Unexpected error"))
          .when(rrReceieverService)
          .handleReportabilityResponse(any(), any());

      ResponseEntity<String> responseEntity =
          reportabilityResponseController.receiveReportabilityResponse(
              xRequestId, xCorrelationId, reportabilityResponse, request, response);
    } catch (ResponseStatusException e) {
      assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, e.getStatusCode());
    }
  }

  @Test
  public void testReSubmitReportabilityResponse_WithValidEicrId() {

    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
    publicHealthMessage.setCdaResponseData("sampleXml");
    publicHealthMessage.setResponseMessageType("RR");
    publicHealthMessage.setxRequestId("12345");

    String eicrId = "validEicrId";

    when(phDao.getBySubmittedMessageId(eicrId)).thenReturn(publicHealthMessage);
    ResponseEntity<String> response =
        reportabilityResponseController.reSubmitReportabilityResponse(eicrId, null);
    assertEquals(HttpStatus.OK, response.getStatusCode());
  }

  @Test
  public void testReSubmitReportabilityResponse_WithValidEicrDocId() {
    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
    publicHealthMessage.setCdaResponseData("sampleXml");
    publicHealthMessage.setResponseMessageType("RR");
    publicHealthMessage.setxRequestId("12345");
    String eicrDocId = "validEicrDocId";

    when(phDao.getBySubmittedDataId(eicrDocId)).thenReturn(publicHealthMessage);

    ResponseEntity<String> response =
        reportabilityResponseController.reSubmitReportabilityResponse(null, eicrDocId);

    assertEquals(HttpStatus.OK, response.getStatusCode());
  }

  @Test
  public void testReSubmitReportabilityResponse_WithMissingEicrIdAndEicrDocId() {

    try {
      ResponseEntity<String> response =
          reportabilityResponseController.reSubmitReportabilityResponse(null, null);
    } catch (ResponseStatusException e) {
      assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, e.getStatusCode());
    }
  }

  @Test
  public void testReSubmitReportabilityResponse_WithException() {

    String eicrId = "validEicrId";

    when(phDao.getBySubmittedMessageId(eicrId)).thenThrow(new RuntimeException("Database error"));
    try {
      ResponseEntity<String> response =
          reportabilityResponseController.reSubmitReportabilityResponse(eicrId, null);
    } catch (ResponseStatusException e) {
      assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, e.getStatusCode());
    }
  }
}
