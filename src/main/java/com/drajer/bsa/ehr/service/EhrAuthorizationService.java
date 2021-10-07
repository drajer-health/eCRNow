package com.drajer.bsa.ehr.service;

import com.drajer.bsa.model.KarProcessingData;

/**
 *
 *
 * <h1>EhrAuthorizationService</h1>
 *
 * This class defines the interface to get authorized with the EHR.
 *
 * @author nbashyam
 */
public interface EhrAuthorizationService {

  /**
   * The method is used to retrieve the access token from the EHR
   *
   * @param kd The processing context which contains information such as patient, encounter,
   *     previous data etc.
   */
  public void getAuthorizationToken(KarProcessingData kd);
}
