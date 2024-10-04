package com.drajer.bsa.ehr.service;

import ca.uhn.fhir.rest.client.interceptor.AdditionalRequestHeadersInterceptor;

/**
 *
 *
 * <h1>EhrHeaderIntercepterInterface</h1>
 *
 * This class defines the interface to get a HTTP Header Intercepter registered with the FHIR Client
 * used to query the EHR's FHIR Server.
 *
 * @author nbashyam
 */
public interface EhrHeaderInterceptorInterface {

  /**
   * The method is used to retrieve the FHIR Client Intercepter. An example of the implementation is
   * present in the com.drajer.bsa.ehr.customizations package.
   *
   * @param kd The ehrLaunchContext that is passed as part of the Launch Patient. This is a
   *     serialized JSON format of the ehrLaunchContext that is passed initially.
   */
  public AdditionalRequestHeadersInterceptor getHeaderInterceptor(String ehrContext);
}
