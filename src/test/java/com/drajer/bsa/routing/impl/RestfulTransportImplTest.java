package com.drajer.bsa.routing.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.auth.RestApiAuthorizationHeaderIf;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.PublicHealthMessage;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

public class RestfulTransportImplTest {

  @InjectMocks private RestfulTransportImpl restfulTransport;

  @Mock private RestApiAuthorizationHeaderIf authorizationService;

  @Mock private RestTemplate restTemplate;

  private KarProcessingData mockData;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    mockData = new KarProcessingData();
    PublicHealthMessage phmData = new PublicHealthMessage();
    phmData.setFhirServerBaseUrl("http://fhirserver.com");
    phmData.setPatientId("12345");
    phmData.setEncounterId("67890");
    phmData.setSubmittedVersionNumber(1);
    phmData.setxCorrelationId("correlation-id");
    mockData.setPhm(phmData);
    mockData.setSubmittedCdaData("<CDA>Data</CDA>");

    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setRestApiUrl("http://api.endpoint.com");
    mockData.setHealthcareSetting(healthcareSetting);
  }

  @Test(expected = RuntimeException.class)
  public void testSendEicrDataUsingRestfulApi_Exception() {

    when(restTemplate.exchange(anyString(), any(), any(), eq(String.class)))
        .thenThrow(new RuntimeException("Error"));

    restfulTransport.sendEicrDataUsingRestfulApi(mockData);
  }

  @Test
  public void testSendEicrDataUsingRestfulApi_Success() {

    HttpHeaders mockHeaders = new HttpHeaders();
    mockHeaders.add("Authorization", "Bearer token");
    when(authorizationService.getAuthorizationHeader(mockData)).thenReturn(mockHeaders);

    ResponseEntity<String> mockResponse = mock(ResponseEntity.class);
    when(mockResponse.getBody()).thenReturn("{\"response\":\"success\"}");
    when(mockResponse.getStatusCodeValue()).thenReturn(200);
    when(restTemplate.exchange(anyString(), any(), any(), eq(String.class)))
        .thenReturn(mockResponse);

    JSONObject response = restfulTransport.sendEicrDataUsingRestfulApi(mockData);

    assertNotNull(response);
    assertEquals("success", response.getString("response"));
    assertEquals(200, response.getInt("status"));
    assertTrue(response.has("response"));
    assertTrue(response.has("status"));
    assertEquals("Bearer token", mockHeaders.getFirst("Authorization"));

    verify(restTemplate, times(1)).exchange(anyString(), any(), any(), eq(String.class));
    verify(authorizationService, times(1)).getAuthorizationHeader(mockData);
  }

  @Test
  public void testSendEicrDataUsingDirect() {
    restfulTransport.sendEicrDataUsingDirect(mockData);

    verifyNoInteractions(restTemplate);
    verifyNoInteractions(authorizationService);
  }

  @Test
  public void testReceiveRrDataUsingDirect() {
    restfulTransport.receiveRrDataUsingDirect(mockData);

    verifyNoInteractions(restTemplate);
    verifyNoInteractions(authorizationService);
  }
}
