package com.drajer.ecrapp.fhir.utils;

import ca.uhn.fhir.rest.gclient.IClientExecutable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FHIRExecuteTemplate {

  @Autowired GenericRetryTemplate fhirRetryTemplate;
  private final Logger logger = LoggerFactory.getLogger(GenericRetryTemplate.class);
  int retryCount = 0;

  public Object execute(IClientExecutable<?, ?> executable) {
    retryCount = 0;
    return fhirRetryTemplate.execute(
        context -> {
          return executeFHIRCall(executable);
        },
        retryContext -> {
          recover();

          return true;
        });
  }

  private Object executeFHIRCall(IClientExecutable<?, ?> executable) {
    retryCount++;
    logger.info("Executing with retry enabled. Count: " + retryCount);

    return executable.execute();
  }

  private void recover() {
    logger.info("Retry failed " + retryCount + " times. Logging Retry Error...");
    retryCount = 0;
  }
}
