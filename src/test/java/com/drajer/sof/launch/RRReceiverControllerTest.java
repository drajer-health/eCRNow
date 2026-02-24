package com.drajer.sof.launch;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;
import com.drajer.ecrapp.service.EicrRRService;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.ResponseEntity;
import org.springframework.web.server.ResponseStatusException;

@RunWith(MockitoJUnitRunner.class)
public class RRReceiverControllerTest {

  @InjectMocks private RRReceiverController rrReceiverController;

  @Mock private EicrRRService rrReceieverService;

  private ReportabilityResponse rrResponse;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    rrResponse = new ReportabilityResponse();
    rrResponse.setResponseType("RR");
    rrResponse.setRrXml("<RR>sample</RR>");
  }

  @Test
  public void testRrReceiver_withValidRR_shouldReturnSuccess() throws Exception {
    ResponseEntity<String> response =
        rrReceiverController.rrReceiver("req-123", "corr-123", rrResponse, true, null, null);

    assertNotNull(response);
    assertEquals("Success", response.getBody());
    assertEquals(200, response.getStatusCodeValue());

    verify(rrReceieverService, times(1)).handleReportabilityResponse(rrResponse, "req-123", true);
  }

  @Test
  public void testRrReceiver_withMDN_shouldCallHandleFailureMdn() {
    rrResponse.setResponseType(Eicr.MDN_RESPONSE_TYPE);

    rrReceiverController.rrReceiver("req-123", "corr-123", rrResponse, true, null, null);

    verify(rrReceieverService, times(1)).handleFailureMdn(rrResponse, "corr-123", "req-123");
  }

  @Test
  public void testReSubmitRR_withEicrId_shouldReturnSuccess() {
    Eicr eicr = new Eicr();
    eicr.setResponseData("<RR>sample</RR>");
    eicr.setResponseType("RR");
    eicr.setxRequestId("req-456");

    when(rrReceieverService.getEicrById(1)).thenReturn(eicr);

    ResponseEntity<String> response = rrReceiverController.reSubmitRR("1", null);

    assertNotNull(response);
    assertEquals("Success", response.getBody());
    assertEquals(200, response.getStatusCodeValue());

    verify(rrReceieverService, times(1))
        .handleReportabilityResponse(any(ReportabilityResponse.class), eq("req-456"), eq(true));
  }

  @Test
  public void testReSubmitRR_withEicrDocId_shouldReturnSuccess() {
    Eicr eicr = new Eicr();
    eicr.setResponseData("<RR>sample</RR>");
    eicr.setResponseType("RR");
    eicr.setxRequestId("req-789");

    when(rrReceieverService.getEicrByDocId("doc-1")).thenReturn(eicr);

    ResponseEntity<String> response = rrReceiverController.reSubmitRR(null, "doc-1");

    assertNotNull(response);
    assertEquals("Success", response.getBody());
    assertEquals(200, response.getStatusCodeValue());

    verify(rrReceieverService, times(1))
        .handleReportabilityResponse(any(ReportabilityResponse.class), eq("req-789"), eq(true));
  }

  @Test
  public void testReSubmitRR_withNonExistingEicr_shouldThrowIllegalArgumentException() {
    when(rrReceieverService.getEicrById(999)).thenReturn(null);

    try {
      rrReceiverController.reSubmitRR("999", null);
      fail("Expected ResponseStatusException to be thrown");
    } catch (ResponseStatusException e) {
      assertEquals(400, e.getStatusCode().value());
      assertTrue(e.getReason().contains("Failed to resubmit the RR, Eicr row not found"));
    }

    verify(rrReceieverService, never())
        .handleReportabilityResponse(any(), anyString(), anyBoolean());
  }

  @Test
  public void testRrReceiver_withMDN_andMissingCorrelationId_shouldThrowInternalServerError() {
    rrResponse.setResponseType(Eicr.MDN_RESPONSE_TYPE);

    try {
      rrReceiverController.rrReceiver("req-123", null, rrResponse, true, null, null);
      fail("Expected ResponseStatusException to be thrown");
    } catch (ResponseStatusException e) {

      assertEquals(500, e.getStatusCode().value());
      assertTrue(e.getReason().contains("X-Correlation-ID header is not present in MDN request."));
    }

    verify(rrReceieverService, never()).handleFailureMdn(any(), anyString(), anyString());
  }

  @Test
  public void testReSubmitRR_withNoEicrIdOrDocId_shouldThrowInternalServerError() {
    try {
      rrReceiverController.reSubmitRR(null, null);
      fail("Expected ResponseStatusException to be thrown");
    } catch (ResponseStatusException e) {

      assertEquals(500, e.getStatusCode().value());
      assertTrue(e.getReason().contains("EicrId or EicrDocId is not present in the request"));
    }

    verify(rrReceieverService, never())
        .handleReportabilityResponse(any(), anyString(), anyBoolean());
  }
}
