package com.drajer.routing;

import static org.junit.Assert.assertNull;

import org.json.JSONObject;
import org.junit.Test;

public class FhirEicrSenderTest {
  @Test
  public void testSubmitBundle_FailurePath() {
    FhirEicrSender sender = new FhirEicrSender();
    JSONObject result = sender.submitBundle("{\"resourceType\":\"Bundle\"}");
    assertNull(result);
  }
}
