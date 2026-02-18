package com.drajer.bsa.controller;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.bsa.service.PhMessageService;
import com.drajer.sof.model.PublicHealthMessageData;
import java.util.*;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@RunWith(MockitoJUnitRunner.class)
public class PhMessageControllerTest {

  @InjectMocks private PhMessageController phMessageController;

  @Mock private PhMessageService phMessageService;

  private PublicHealthMessage message1;
  private PublicHealthMessage message2;

  @Before
  public void setUp() {
    message1 = mock(PublicHealthMessage.class);
    message2 = mock(PublicHealthMessage.class);
  }

  @Test
  public void testGetPhMessageDetails_Success() {
    List<PublicHealthMessage> mockList = Arrays.asList(message1, message2);
    when(phMessageService.getPhMessageData(any(Map.class), eq(false))).thenReturn(mockList);

    ResponseEntity<Object> response =
        phMessageController.getPhMessageDetails(
            "http://fhirserver",
            "patient123",
            "encounter456",
            "req1",
            "sub1",
            "v1",
            "resp1",
            "inst",
            "res1",
            "Observation",
            "kar1",
            "notif1",
            "corr1",
            "2025-10-10T10:00",
            "2025-10-10T12:00",
            false);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertTrue(((List<?>) response.getBody()).containsAll(mockList));
    assertNotNull(response.getBody());
    assertFalse(((List<?>) response.getBody()).isEmpty());
    assertSame(mockList, response.getBody());
    verify(phMessageService, times(1)).getPhMessageData(any(Map.class), eq(false));
  }

  @Test
  public void testGetPhMessageDetails_NoData() {
    when(phMessageService.getPhMessageData(any(Map.class), eq(false))).thenReturn(null);
    ResponseEntity<Object> response =
        phMessageController.getPhMessageDetails(
            null, null, null, null, null, null, null, null, null, null, null, null, null, null,
            null, false);
    verify(phMessageService, times(1)).getPhMessageData(any(Map.class), eq(false));
    assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
  }

  @Test
  public void testGetPhMessageDetails_Exception() {
    when(phMessageService.getPhMessageData(any(Map.class), eq(false)))
        .thenThrow(new RuntimeException("fail"));
    try {
      phMessageController.getPhMessageDetails(
          null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,
          false);
      fail();
    } catch (Exception ex) {
      assertTrue(ex instanceof org.springframework.web.server.ResponseStatusException);
      assertEquals(
          HttpStatus.BAD_REQUEST,
          ((org.springframework.web.server.ResponseStatusException) ex).getStatusCode());
    }
    verify(phMessageService, times(1)).getPhMessageData(any(Map.class), eq(false));
  }

  @Test
  public void testGetPhMessageDetails_AllFieldsEmpty() {
    when(phMessageService.getPhMessageData(any(Map.class), eq(false))).thenReturn(null);

    ResponseEntity<Object> response =
        phMessageController.getPhMessageDetails(
            "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", false);

    verify(phMessageService, times(1)).getPhMessageData(any(Map.class), eq(false));
    assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
  }

  @Test
  public void testGetPhMessagesContainingXRequestIds_EmptyListFromService() {
    Map<String, Object> request = new HashMap<>();
    request.put("xRequestIds", Arrays.asList("req1"));
    request.put("summaryFlag", false);

    when(phMessageService.getPhMessagesContainingXRequestIds(anyList(), eq(false)))
        .thenReturn(Collections.emptyList());

    ResponseEntity<Object> response =
        phMessageController.getPhMessagesContainingXRequestIds(request);

    assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
    assertEquals("No phMessage records found.", response.getBody());
    verify(phMessageService, times(1)).getPhMessagesContainingXRequestIds(anyList(), eq(false));
  }

  @Test
  public void testGetPhMessageDetails_AllParamsNullOrEmpty() {
    when(phMessageService.getPhMessageData(any(Map.class), eq(false))).thenReturn(null);
    ResponseEntity<Object> response =
        phMessageController.getPhMessageDetails(
            null, "", null, "", null, "", null, "", null, "", "", null, null, null, null, false);
    verify(phMessageService, times(1)).getPhMessageData(any(Map.class), eq(false));
    assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
  }

  @Test
  public void testGetPhMessageDetailsSummary_Success() {
    List<PublicHealthMessage> mockList = Collections.singletonList(message1);
    when(phMessageService.getPhMessageDataSummary(any(Map.class))).thenReturn(mockList);

    ResponseEntity<Object> response =
        phMessageController.getPhMessageDetailsSummary(
            "http://fhirserver",
            "patient123",
            "encounter456",
            "req1",
            "sub1",
            "v1",
            "resp1",
            "inst",
            "res1",
            "Observation",
            "kar1",
            "notif1",
            "corr1",
            "2025-10-10T10:00",
            "2025-10-10T12:00");

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertTrue(((List<?>) response.getBody()).containsAll(mockList));
    verify(phMessageService, times(1)).getPhMessageDataSummary(any(Map.class));
  }

  @Test
  public void testGetPhMessageDetailsSummary_NoData() {
    when(phMessageService.getPhMessageDataSummary(any(Map.class))).thenReturn(null);
    ResponseEntity<Object> response =
        phMessageController.getPhMessageDetailsSummary(
            null, null, null, null, null, null, null, null, null, null, null, null, null, null,
            null);
    verify(phMessageService, times(1)).getPhMessageDataSummary(any(Map.class));
    assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
  }

  @Test
  public void testGetPhMessageDetailsSummary_Exception() {
    when(phMessageService.getPhMessageDataSummary(any(Map.class)))
        .thenThrow(new RuntimeException("fail"));
    try {
      phMessageController.getPhMessageDetailsSummary(
          null, null, null, null, null, null, null, null, null, null, null, null, null, null, null);
      fail();
    } catch (Exception ex) {
      assertTrue(ex instanceof org.springframework.web.server.ResponseStatusException);
      assertEquals(
          HttpStatus.BAD_REQUEST,
          ((org.springframework.web.server.ResponseStatusException) ex).getStatusCode());
    }
    verify(phMessageService, times(1)).getPhMessageDataSummary(any(Map.class));
  }

  @Test
  public void testGetByBatchXRequestIds_Success() {
    List<PublicHealthMessage> mockList = Arrays.asList(message1, message2);
    Map<String, Object> request = new HashMap<>();
    request.put("xRequestIds", Arrays.asList("req1", "req2"));
    request.put("summaryFlag", false);

    when(phMessageService.getPhMessageDataByXRequestIds(anyList(), eq(false))).thenReturn(mockList);

    ResponseEntity<Object> response = phMessageController.getByBatchXRequestIds(request);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertTrue(((List<?>) response.getBody()).containsAll(mockList));
    verify(phMessageService, times(1)).getPhMessageDataByXRequestIds(anyList(), eq(false));
  }

  @Test
  public void testGetByBatchXRequestIds_Exception() {
    Map<String, Object> request = new HashMap<>();
    request.put("xRequestIds", Arrays.asList("req1", "req2"));
    request.put("summaryFlag", false);

    when(phMessageService.getPhMessageDataByXRequestIds(anyList(), eq(false)))
        .thenThrow(new RuntimeException("fail"));

    try {
      phMessageController.getByBatchXRequestIds(request);
      fail();
    } catch (Exception ex) {
      assertTrue(ex instanceof org.springframework.web.server.ResponseStatusException);
    }
    verify(phMessageService, times(1)).getPhMessageDataByXRequestIds(anyList(), eq(false));
  }

  @Test
  public void testGetByBatchXRequestIds_EmptyListFromService() {
    Map<String, Object> request = new HashMap<>();
    request.put("xRequestIds", Arrays.asList("req1", "req2"));
    request.put("summaryFlag", false);

    when(phMessageService.getPhMessageDataByXRequestIds(anyList(), eq(false)))
        .thenReturn(Collections.emptyList());

    ResponseEntity<Object> response = phMessageController.getByBatchXRequestIds(request);

    assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
    assertEquals("No phMessage records found.", response.getBody());
    verify(phMessageService, times(1)).getPhMessageDataByXRequestIds(anyList(), eq(false));
  }

  @Test
  public void testGetPhMessagesContainingXRequestIds_EmptyIds() {
    Map<String, Object> request = new HashMap<>();
    request.put("xRequestIds", Collections.emptyList());
    request.put("summaryFlag", false);

    ResponseEntity<Object> response =
        phMessageController.getPhMessagesContainingXRequestIds(request);
    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    verify(phMessageService, never()).getPhMessagesContainingXRequestIds(anyList(), anyBoolean());
  }

  @Test
  public void testGetPhMessagesContainingXRequestIds_Exception() {
    Map<String, Object> request = new HashMap<>();
    request.put("xRequestIds", Arrays.asList("req1"));
    request.put("summaryFlag", false);

    when(phMessageService.getPhMessagesContainingXRequestIds(anyList(), eq(false)))
        .thenThrow(new RuntimeException("fail"));

    try {
      phMessageController.getPhMessagesContainingXRequestIds(request);
      fail();
    } catch (Exception ex) {
      assertTrue(ex instanceof org.springframework.web.server.ResponseStatusException);
    }
    verify(phMessageService, times(1)).getPhMessagesContainingXRequestIds(anyList(), eq(false));
  }

  @Test
  public void testDeletePhMessages_Success() {
    PublicHealthMessageData data = new PublicHealthMessageData();
    data.setFhirServerBaseUrl("http://fhirserver");
    data.setPatientId("patient123");

    List<PublicHealthMessage> mockList = Arrays.asList(message1, message2);
    when(phMessageService.getPhMessageByParameters(data)).thenReturn(mockList);

    ResponseEntity<String> response = phMessageController.deletePhMessages(data);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals("phMessages deleted successfully", response.getBody());
    assertNotNull(response.getBody());
    assertFalse(response.getBody().isEmpty());
    verify(phMessageService).deletePhMessage(message1);
    verify(phMessageService).deletePhMessage(message2);
  }

  @Test
  public void testDeletePhMessages_NullInput() {
    ResponseEntity<String> response = phMessageController.deletePhMessages(null);
    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
  }

  @Test
  public void testDeletePhMessages_Exception() {
    PublicHealthMessageData data = new PublicHealthMessageData();
    data.setFhirServerBaseUrl("http://fhirserver");
    data.setPatientId("patient123");

    when(phMessageService.getPhMessageByParameters(data)).thenThrow(new RuntimeException("fail"));
    ResponseEntity<String> response = phMessageController.deletePhMessages(data);
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
    verify(phMessageService, times(1)).getPhMessageByParameters(data);
  }

  @Test
  public void testGetByBatchXRequestIds_NullListInsideMap() {
    Map<String, Object> request = new HashMap<>();
    request.put("xRequestIds", null);
    request.put("summaryFlag", false);
    ResponseEntity<Object> response = phMessageController.getByBatchXRequestIds(request);
    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
  }

  @Test
  public void testGetPhMessagesContainingXRequestIds_SingleElement() {
    Map<String, Object> request = new HashMap<>();
    request.put("xRequestIds", Collections.singletonList("reqSingle"));
    request.put("summaryFlag", false);
    List<PublicHealthMessage> list = Collections.singletonList(message1);
    when(phMessageService.getPhMessagesContainingXRequestIds(anyList(), eq(false)))
        .thenReturn(list);
    ResponseEntity<Object> response =
        phMessageController.getPhMessagesContainingXRequestIds(request);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertTrue(((List<?>) response.getBody()).contains(message1));
    verify(phMessageService, times(1)).getPhMessagesContainingXRequestIds(anyList(), eq(false));
  }

  @Test
  public void testDeletePhMessages_EmptyListFromService() {
    PublicHealthMessageData data = new PublicHealthMessageData();
    data.setFhirServerBaseUrl("url");
    data.setPatientId("p");
    when(phMessageService.getPhMessageByParameters(data)).thenReturn(Collections.emptyList());
    ResponseEntity<String> response = phMessageController.deletePhMessages(data);
    assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
    verify(phMessageService, times(1)).getPhMessageByParameters(data);
  }
}
