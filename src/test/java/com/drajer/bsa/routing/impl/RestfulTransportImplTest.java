package com.drajer.bsa.routing.impl;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.PublicHealthMessage;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

@RunWith(MockitoJUnitRunner.class)
public class RestfulTransportImplTest {

  @InjectMocks RestfulTransportImpl restfulTransportImpl;

  @Mock PublicHealthMessage publicHealthMessage;

  @Mock private KarProcessingData data;

  @Mock private HealthcareSetting healthCareSetting;

  @Mock private RestTemplate restTemplate;

  @Mock private ResponseEntity<String> response;

  @Captor private ArgumentCaptor<String> urlCaptor;

  @Captor private ArgumentCaptor<HttpMethod> httpMethodCaptor;

  @Captor private ArgumentCaptor<HttpEntity<String>> httpEntityCaptor;

  @Captor private ArgumentCaptor<Class<String>> clazzCaptor;

  private static final String MATCHED_PATHS_HEADER = "matchedPaths";

  private static final String EICR_DOC_ID_HEADER = "eicrDocId";

  private static final String submitted_data_id = "Sample_submitted_data_id";
  private static final String matched_path =
      "ServiceRequest.code,ServiceRequest.code,ServiceRequest.code,ServiceRequest.code,ServiceRequest.code";
  private static final String status =
      "{\"statuses\":[{\"actionId\":\"\",\"actionType\":\"UNKNOWN\",\"actionStatus\":\"NOT_STARTED\",\"outputProduced\":[],\"triggerMatchStatus\":true,\"matchedCodes\":[{\"matchedCodes\":[\"http://loinc.org|94762-2\"],\"matchedValues\":[],\"valueSet\":\"http://ersd.aimsplatform.org/fhir/ValueSet/lotc\",\"valueSetVersion\":\"2022-10-19\",\"matchedPath\":\"ServiceRequest.code\"},{\"matchedCodes\":[\"http://loinc.org|94500-6\"],\"matchedValues\":[],\"valueSet\":\"http://ersd.aimsplatform.org/fhir/ValueSet/lotc\",\"valueSetVersion\":\"2022-10-19\",\"matchedPath\":\"ServiceRequest.code\"},{\"matchedCodes\":[\"http://loinc.org|94762-2\"],\"matchedValues\":[],\"valueSet\":\"http://ersd.aimsplatform.org/fhir/ValueSet/lotc\",\"valueSetVersion\":\"2022-10-19\",\"matchedPath\":\"ServiceRequest.code\"},{\"matchedCodes\":[\"http://loinc.org|94762-2\"],\"matchedValues\":[],\"valueSet\":\"http://ersd.aimsplatform.org/fhir/ValueSet/lotc\",\"valueSetVersion\":\"2022-10-19\",\"matchedPath\":\"ServiceRequest.code\"},{\"matchedCodes\":[\"http://loinc.org|94762-2\"],\"matchedValues\":[],\"valueSet\":\"http://ersd.aimsplatform.org/fhir/ValueSet/lotc\",\"valueSetVersion\":\"2022-10-19\",\"matchedPath\":\"ServiceRequest.code\"}],\"matchedResources\":null}]}";

  @Test
  public void sendEicrDataUsingRestfulApiTest() {

    setUp();
    restfulTransportImpl.sendEicrDataUsingRestfulApi(data);
    verify(restTemplate)
        .exchange(
            urlCaptor.capture(),
            httpMethodCaptor.capture(),
            httpEntityCaptor.capture(),
            clazzCaptor.capture());

    assertEquals(
        Boolean.TRUE, httpEntityCaptor.getValue().getHeaders().containsKey(MATCHED_PATHS_HEADER));
    assertEquals(
        matched_path, httpEntityCaptor.getValue().getHeaders().get(MATCHED_PATHS_HEADER).get(0));
    assertEquals(
        Boolean.TRUE, httpEntityCaptor.getValue().getHeaders().containsKey(EICR_DOC_ID_HEADER));
  }

  private void setUp() {

    ReflectionTestUtils.setField(
        restfulTransportImpl, "isMatchedPathsAndEicrDocIdRequired", Boolean.TRUE);
    when(data.getPhm()).thenReturn(publicHealthMessage);
    when(publicHealthMessage.getxCorrelationId()).thenReturn("corr456");
    when(publicHealthMessage.getTriggerMatchStatus()).thenReturn(status);
    when(publicHealthMessage.getSubmittedDataId()).thenReturn(submitted_data_id);

    when(data.getHealthcareSetting()).thenReturn(healthCareSetting);
    when(healthCareSetting.getRestApiUrl()).thenReturn("/api/outbound/messages");
    when(restTemplate.exchange(
            ArgumentMatchers.anyString(),
            ArgumentMatchers.any(HttpMethod.class),
            ArgumentMatchers.any(),
            ArgumentMatchers.<Class<String>>any()))
        .thenReturn(response);
    when(response.getBody()).thenReturn("{\"response\":\"bundleReponseTosend\"}");
    when(response.getStatusCodeValue()).thenReturn(HttpStatus.OK.value());
  }
}
