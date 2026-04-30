package com.drajer.bsa.controller;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;

import com.drajer.bsa.model.RestApiBody;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

public class SampleRestApiReceiverTest {
  private SampleRestApiReceiver receiver;

  @Mock private HttpServletRequest request;

  @Mock private HttpServletResponse response;

  private RestApiBody testBody;

  @Before
  public void setup() {

    MockitoAnnotations.initMocks(this);

    receiver = new SampleRestApiReceiver();

    testBody = new RestApiBody();
    testBody.setFhirServerURL("http://fhirserver.com");
    testBody.setPatientId("patient123");
    testBody.setEncounterId("encounter123");
    testBody.setSubmittedVersionId(true);
    testBody.setPayload("sample-payload");

    when(request.getHeader("X-Request-ID")).thenReturn("12345");
    when(request.getHeader("Authorization")).thenReturn("Bearer token");
  }

  @Test
  public void testReceiveEicr() throws JSONException {

    ResponseEntity<Object> responseEntity = receiver.receiveEicr(testBody, request, response);

    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

    JSONObject responseObject = new JSONObject(responseEntity.getBody().toString());

    assertEquals("Success", responseObject.getString("status"));
  }
}
