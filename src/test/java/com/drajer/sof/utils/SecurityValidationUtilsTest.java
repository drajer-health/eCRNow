package com.drajer.sof.utils;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class SecurityValidationUtilsTest {

  @Test
  public void testValidateUrl_acceptsAbsoluteHttpAndHttpsEndpoints() {
    assertTrue(
        SecurityValidationUtils.validateUrl(
            "https://fhir-ehr.sandboxcerner.com/r4/0b8a0111-e8e6-4c26-a91c-5069cbc6b1ca"));
    assertTrue(SecurityValidationUtils.validateUrl("http://localhost:9011/FHIR"));
    assertTrue(SecurityValidationUtils.validateUrl("HTTPS://FHIR-EHR.SANDBOXCERNER.COM/dstu2"));
  }

  @Test
  public void testValidateUrl_rejectsEverythingElse() {
    assertFalse("null", SecurityValidationUtils.validateUrl(null));
    assertFalse("blank", SecurityValidationUtils.validateUrl("   "));
    assertFalse("relative path", SecurityValidationUtils.validateUrl("/r4/Patient/123"));
    assertFalse(
        "protocol-relative",
        SecurityValidationUtils.validateUrl("//fhir-ehr.sandboxcerner.com/r4"));
    assertFalse("non-http scheme", SecurityValidationUtils.validateUrl("file:///etc/passwd"));
    assertFalse(
        "unparseable", SecurityValidationUtils.validateUrl("http://fhir ehr.sandboxcerner.com"));
  }
}
