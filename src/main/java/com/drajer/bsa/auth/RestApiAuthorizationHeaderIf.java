package com.drajer.bsa.auth;

import com.drajer.bsa.model.KarProcessingData;
import org.springframework.http.HttpHeaders;

/**
 *
 *
 * <h1>RestApiAuthorizationHeaderIf>/h1>
 *
 * <p>The interface exposes a method that can be overridden and implemented by Healthcare
 * Organizations or EHR vendors to inject an Authorization Header prior to invoking REST API to hand
 * off the Payload (e.g eICR) for routing. This allows the healthcare organization to protect the
 * REST API and verify there is always a valid authorization header token before allowing the call
 * the proceed. Since the eCRNow is not aware of the security models being used by the healthcare
 * organization, this interface allows for customization at the EHR/Healthcare organization.
 *
 * @author nbashyam
 */
public interface RestApiAuthorizationHeaderIf {

  public HttpHeaders getAuthorizationHeader(KarProcessingData data);
}
