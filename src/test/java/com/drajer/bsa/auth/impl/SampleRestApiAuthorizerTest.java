package com.drajer.bsa.auth.impl;

import static org.junit.Assert.assertTrue;

import com.drajer.bsa.model.KarProcessingData;
import org.junit.Test;
import org.springframework.http.HttpHeaders;

public class SampleRestApiAuthorizerTest {
  @Test
  public void getAuthorizationHeadertest() {
    KarProcessingData karProcessingData = new KarProcessingData();
    SampleRestApiAuthorizer sampleRestApiAuthorizer = new SampleRestApiAuthorizer();
    HttpHeaders result = sampleRestApiAuthorizer.getAuthorizationHeader(karProcessingData);
    assertTrue(result.containsKey(HttpHeaders.AUTHORIZATION));
  }
}
