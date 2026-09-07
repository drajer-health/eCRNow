package com.drajer.routing;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.sof.utils.Authorization;
import com.drajer.sof.utils.FhirContextInitializer;
import org.json.JSONObject;
import org.junit.Test;

public class FhirEicrSenderTest {
  @Test
  public void testSubmitBundle_FailurePath() {
    FhirContextInitializer contextInitializer = mock(FhirContextInitializer.class);
    Authorization authorization = mock(Authorization.class);
    String fhirServerURL = "http://localhost:8080/fhir";

    FhirEicrSender sender = new FhirEicrSender(contextInitializer, authorization, fhirServerURL);
    JSONObject result = sender.submitBundle("{\"resourceType\":\"Bundle\"}");
    assertNull(result);
  }

  @Test
  public void testSubmitBundle_WithInvalidBundleFormat() {
    // Test that submitBundle handles malformed JSON gracefully
    FhirContextInitializer contextInitializer = mock(FhirContextInitializer.class);
    Authorization authorization = mock(Authorization.class);
    String fhirServerURL = "http://localhost:8080/fhir";

    FhirEicrSender sender = new FhirEicrSender(contextInitializer, authorization, fhirServerURL);

    // Send invalid bundle format - should return null without throwing exception
    JSONObject result = sender.submitBundle("invalid json {");
    assertNull(result);
  }
}
