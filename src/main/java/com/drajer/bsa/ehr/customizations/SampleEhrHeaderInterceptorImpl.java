package com.drajer.bsa.ehr.customizations;

import ca.uhn.fhir.rest.client.interceptor.AdditionalRequestHeadersInterceptor;
import com.drajer.bsa.ehr.service.EhrHeaderInterceptorInterface;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Map;
import org.springframework.stereotype.Service;

@Service
public class SampleEhrHeaderInterceptorImpl implements EhrHeaderInterceptorInterface {

  @Override
  public AdditionalRequestHeadersInterceptor getHeaderInterceptor(String ehrContext) {

    AdditionalRequestHeadersInterceptor interceptor = null;

    if (ehrContext != null && !ehrContext.isEmpty()) {

      ObjectMapper objectMapper = new ObjectMapper();
      try {
        Map<String, String> ehrLaunchContext = objectMapper.readValue(ehrContext, Map.class);

        if (ehrLaunchContext != null && !ehrLaunchContext.isEmpty()) {
          interceptor = new AdditionalRequestHeadersInterceptor();

          for (Map.Entry<String, String> entry : ehrLaunchContext.entrySet()) {

            interceptor.addHeaderValue(entry.getKey(), entry.getValue());
          }
        }
      } catch (JsonProcessingException e) {
        // Handle Error here
      }
    }

    return interceptor;
  }
}
