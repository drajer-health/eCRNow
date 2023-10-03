package com.drajer.routing.impl;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.routing.RestApiSender;
import com.drajer.sof.model.LaunchDetails;
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
public class MatchedCodesAddedToHeaderTest {

  @Mock private LaunchDetails launchDetails;

  @InjectMocks private RestApiSender restApiSender;

  @Mock private Eicr ecr;

  @Mock private RestTemplate restTemplate;

  @Mock private ResponseEntity<String> response;

  @Captor private ArgumentCaptor<String> urlCaptor;

  @Captor private ArgumentCaptor<HttpMethod> httpMethodCaptor;

  @Captor private ArgumentCaptor<HttpEntity<String>> httpEntityCaptor;

  @Captor private ArgumentCaptor<Class<String>> clazzCaptor;

  private static final String MATCHED_PATHS_HEADER = "matchedPaths";

  private static final String EICR_DOC_ID_HEADER = "eicrDocId";

  private static final String eicrDocId = "sampleEicrDocID";
  private static final Object MATCHED_CODES =
      "http://hl7.org/fhir/sid/icd-10-cm|B34.2http://hl7.org/fhir/sid/icd-10-cm|U07.1http://snomed.info/sct|840539006http://loinc.org|94500-6http://loinc.org|94762-2http://loinc.org|94500-6";
  private static final String matched_path = "Condition.code,ServiceRequest.code,Observation.code";
  private static final String status =
      "{\"patientId\":\"11111111\",\"encounterId\":\"22222222\",\"matchTriggerStatus\":{\"actionId\":\"match-trigger\",\"jobStatus\":\"COMPLETED\",\"triggerMatchStatus\":true,\"matchedCodes\":[{\"matchedCodes\":[\"http://hl7.org/fhir/sid/icd-10-cm|B34.2\",\"http://hl7.org/fhir/sid/icd-10-cm|U07.1\",\"http://snomed.info/sct|840539006\"],\"matchedValues\":null,\"valueSet\":\"2.16.840.1.113762.1.4.1146.1123\",\"valueSetVersion\":\"1\",\"matchedPath\":\"Condition.code\"},{\"matchedCodes\":[\"http://loinc.org|94500-6\",\"http://loinc.org|94762-2\"],\"matchedValues\":null,\"valueSet\":\"2.16.840.1.113762.1.4.1146.1123\",\"valueSetVersion\":\"1\",\"matchedPath\":\"ServiceRequest.code\"},{\"matchedCodes\":[\"http://loinc.org|94500-6\"],\"matchedValues\":null,\"valueSet\":\"2.16.840.1.113762.1.4.1146.1123\",\"valueSetVersion\":\"1\",\"matchedPath\":\"Observation.code\"}],\"triggerLastExecutionDateTime\":1690457083714},\"createEicrStatus\":{\"actionId\":\"\",\"jobStatus\":\"COMPLETED\",\"eicrCreated\":true,\"eICRId\":\"247\"},\"periodicUpdateStatus\":[{\"actionId\":\"periodic-update-eicr\",\"jobStatus\":\"SKIPPED\",\"eicrUpdated\":false,\"eICRId\":\"0\"}],\"periodicUpdateJobStatus\":\"SUSPENDED\",\"closeOutEicrStatus\":{\"actionId\":\"\",\"jobStatus\":\"NOT_STARTED\",\"eicrClosed\":false,\"eICRId\":\"\"},\"validateEicrStatus\":[{\"actionId\":\"validate-eicr\",\"jobStatus\":\"COMPLETED\",\"eicrValidated\":true,\"eICRId\":\"247\",\"validationTime\":1690457092937}],\"submitEicrStatus\":[{\"actionId\":\"route-and-send-eicr\",\"jobStatus\":\"COMPLETED\",\"eicrSubmitted\":true,\"eICRId\":\"247\",\"submittedTime\":1690457093089,\"transportUsed\":null}],\"rrStatus\":[{\"actionId\":\"8d1758e1-6986-4a7d-9ac9-41f319aa912c\",\"jobStatus\":\"COMPLETED\",\"rrObtained\":true,\"eICRId\":\"247\",\"rrTime\":1690457093095,\"transportUsed\":null}],\"eicrsForRRCheck\":[],\"eicrsReadyForValidation\":[],\"eicrsReadyForSubmission\":[]}";

  @Test
  public void getTriggerMatchedCodesTest() {

    setup();
    restApiSender.sendEicrXmlDocument(launchDetails, "eicrXml", ecr);
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
    assertEquals(
        eicrDocId, httpEntityCaptor.getValue().getHeaders().get(EICR_DOC_ID_HEADER).get(0));
  }

  private void setup() {
    ReflectionTestUtils.setField(restApiSender, "isMatchedPathsAndEicrDocIdRequired", Boolean.TRUE);
    when(launchDetails.getStatus()).thenReturn(status);
    when(launchDetails.getRestAPIURL()).thenReturn("/api/outbound/messages");
    when(restTemplate.exchange(
            ArgumentMatchers.anyString(),
            ArgumentMatchers.any(HttpMethod.class),
            ArgumentMatchers.any(),
            ArgumentMatchers.<Class<String>>any()))
        .thenReturn(response);
    when(response.getBody()).thenReturn("{\"response\":\"bundleReponseTosend\"}");
    when(response.getStatusCodeValue()).thenReturn(HttpStatus.OK.value());
    when(ecr.getEicrDocId()).thenReturn(eicrDocId);
  }
}
