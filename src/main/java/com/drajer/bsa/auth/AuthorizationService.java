package com.drajer.bsa.auth;

import com.drajer.bsa.model.FhirServerDetails;
import org.json.JSONObject;

/**
 *
 *
 * <h1>EhrAuthorizationService</h1>
 *
 * This class defines the interface to get authorized with the EHR.
 *
 * @author nbashyam
 */
public interface AuthorizationService {

  /**
   * The method is used to retrieve the access token from the EHR
   *
   * @param fsd The processing context which contains information such as patient, encounter,
   *     previous data etc.
   */
  JSONObject getAuthorizationToken(FhirServerDetails fsd);
}
