package com.drajer.sof.launch;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.ClientDetailsDTO;
import com.drajer.sof.service.ClientDetailsService;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.util.Arrays;
import java.util.List;
import org.apache.commons.text.StringEscapeUtils;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@RunWith(PowerMockRunner.class)
@PrepareForTest({StringEscapeUtils.class})
public class ClientDetailsControllerTest {

  @InjectMocks private ClientDetailsController controller;

  @Mock private ClientDetailsService clientDetailsService;

  @Mock private HttpServletRequest request;

  @Mock private HttpServletResponse response;

  private ClientDetails clientDetails;
  private ClientDetailsDTO clientDetailsDTO;

  @Before
  public void setUp() {
    clientDetails = new ClientDetails();
    clientDetails.setId(1);
    clientDetails.setFhirServerBaseURL("http://test.com");
    clientDetails.setIsEmergentReportingEnabled(true);

    clientDetailsDTO = new ClientDetailsDTO();
    clientDetailsDTO.setId(1);
    clientDetailsDTO.setFhirServerBaseURL("http://test.com");
    clientDetailsDTO.setIsEmergentReportingEnabled(true);
  }

  @Test
  public void testGetClientDetailsById() {
    when(clientDetailsService.getClientDetailsById(1)).thenReturn(clientDetails);
    ClientDetails result = controller.getClientDetailsById(1);
    assertNotNull(result);
    assertEquals("http://test.com", result.getFhirServerBaseURL());
    verify(clientDetailsService, times(1)).getClientDetailsById(1);
  }

  @Test
  public void testCreateClientDetails_NewClient_Success() {
    when(clientDetailsService.getClientDetailsByUrl("http://test.com")).thenReturn(null);

    ResponseEntity<Object> response = controller.createClientDetails(clientDetailsDTO);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertTrue(response.getBody() instanceof ClientDetailsDTO);
    verify(clientDetailsService, times(1)).saveOrUpdate(any(ClientDetails.class));
  }

  @Test
  public void testCreateClientDetails_UrlAlreadyRegistered() throws JSONException {
    when(clientDetailsService.getClientDetailsByUrl("http://test.com")).thenReturn(clientDetails);

    ResponseEntity<Object> response = controller.createClientDetails(clientDetailsDTO);

    assertEquals(HttpStatus.CONFLICT, response.getStatusCode());
    JSONObject body = new JSONObject(response.getBody().toString());
    assertEquals("error", body.get("status"));
    verify(clientDetailsService, never()).saveOrUpdate(any(ClientDetails.class));
  }

  @Test
  public void testUpdateClientDetails_NewClient_Success() {
    when(clientDetailsService.getClientDetailsByUrl("http://test.com")).thenReturn(null);

    ResponseEntity<Object> response = controller.updateClientDetails(clientDetailsDTO);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertTrue(response.getBody() instanceof ClientDetailsDTO);
    verify(clientDetailsService, times(1)).saveOrUpdate(any(ClientDetails.class));
  }

  @Test
  public void testUpdateClientDetails_SameId_Success() {
    when(clientDetailsService.getClientDetailsByUrl("http://test.com")).thenReturn(clientDetails);

    ResponseEntity<Object> response = controller.updateClientDetails(clientDetailsDTO);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    verify(clientDetailsService, times(1)).saveOrUpdate(any(ClientDetails.class));
  }

  @Test
  public void testUpdateClientDetails_UrlAlreadyRegistered() throws JSONException {
    ClientDetails otherClient = new ClientDetails();
    otherClient.setId(2);
    otherClient.setFhirServerBaseURL("http://test.com");

    when(clientDetailsService.getClientDetailsByUrl("http://test.com")).thenReturn(otherClient);

    ResponseEntity<Object> response = controller.updateClientDetails(clientDetailsDTO);

    assertEquals(HttpStatus.CONFLICT, response.getStatusCode());
    JSONObject body = new JSONObject(response.getBody().toString());
    assertEquals("error", body.get("status"));
    verify(clientDetailsService, never()).saveOrUpdate(any(ClientDetails.class));
  }

  @Test
  public void testGetClientDetailsByUrl() {
    when(clientDetailsService.getClientDetailsByUrl("http://test.com")).thenReturn(clientDetails);
    ClientDetails result = controller.getClientDetailsByUrl("http://test.com");
    assertNotNull(result);
    assertEquals("http://test.com", result.getFhirServerBaseURL());
    verify(clientDetailsService, times(1)).getClientDetailsByUrl("http://test.com");
  }

  @Test
  public void testGetAllClientDetails() {
    List<ClientDetails> list = Arrays.asList(clientDetails);
    when(clientDetailsService.getAllClientDetails()).thenReturn(list);

    List<ClientDetails> result = controller.getAllClientDetails();

    assertEquals(1, result.size());
    assertEquals("http://test.com", result.get(0).getFhirServerBaseURL());
    verify(clientDetailsService, times(1)).getAllClientDetails();
  }

  @Test
  public void testDeleteClientDetails_Success() {
    mockStatic(StringEscapeUtils.class);
    when(StringEscapeUtils.escapeJava(anyString())).thenAnswer(i -> i.getArgument(0));
    when(clientDetailsService.getClientDetailsByUrl("http://test.com")).thenReturn(clientDetails);

    ResponseEntity<String> responseEntity =
        controller.deleteClientDetails("http://test.com", "req-id", "corr-id", request, response);

    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    assertEquals("ClientDetails deleted successfully", responseEntity.getBody());
    verify(clientDetailsService, times(1)).delete(any(ClientDetails.class));
  }

  @Test
  public void testDeleteClientDetails_MissingUrl() {
    ResponseEntity<String> responseEntity =
        controller.deleteClientDetails("", "req-id", "corr-id", request, response);

    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    assertTrue(responseEntity.getBody().contains("missing or empty"));
    verify(clientDetailsService, never()).delete(any(ClientDetails.class));
  }

  @Test
  public void testDeleteClientDetails_NotFound() {
    mockStatic(StringEscapeUtils.class);
    when(StringEscapeUtils.escapeJava(anyString())).thenAnswer(i -> i.getArgument(0));
    when(clientDetailsService.getClientDetailsByUrl("http://notfound.com")).thenReturn(null);

    ResponseEntity<String> responseEntity =
        controller.deleteClientDetails(
            "http://notfound.com", "req-id", "corr-id", request, response);

    assertEquals(HttpStatus.NOT_FOUND, responseEntity.getStatusCode());
    assertTrue(responseEntity.getBody().contains("Not found"));
    verify(clientDetailsService, never()).delete(any(ClientDetails.class));
  }

  @Test
  public void testDeleteClientDetails_Exception() {
    mockStatic(StringEscapeUtils.class);
    when(StringEscapeUtils.escapeJava(anyString())).thenAnswer(i -> i.getArgument(0));
    when(clientDetailsService.getClientDetailsByUrl(anyString()))
        .thenThrow(new RuntimeException("DB error"));

    ResponseEntity<String> responseEntity =
        controller.deleteClientDetails("http://fail.com", "req-id", "corr-id", request, response);

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, responseEntity.getStatusCode());
    assertEquals(ClientDetailsController.ERROR_IN_PROCESSING_THE_REQUEST, responseEntity.getBody());
  }
}
