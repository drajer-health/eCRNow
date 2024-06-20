package com.drajer.bsa.utils;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Date;
import org.hl7.fhir.r4.model.InstantType;
import org.hl7.fhir.r4.model.Meta;
import org.hl7.fhir.r4.model.OperationOutcome;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class OperationOutcomeUtil {

  private static final Logger logger = LoggerFactory.getLogger(OperationOutcomeUtil.class);

  private static final ObjectMapper objectMapper = new ObjectMapper();

  public static Object createOperationOutcome(String message) {

    IParser parser = FhirContext.forR4().newJsonParser();
    OperationOutcome operationOutcome = new OperationOutcome();
    operationOutcome
        .addIssue()
        .setSeverity(OperationOutcome.IssueSeverity.ERROR)
        .setCode(OperationOutcome.IssueType.EXCEPTION)
        .setDiagnostics(message);
    Meta meta = new Meta();
    meta.setLastUpdatedElement(new InstantType());
    meta.getLastUpdatedElement().setValue(new Date()); // Set current date/time

    operationOutcome.setMeta(meta);

    String json = null;
    try {
      json = parser.encodeResourceToString(operationOutcome);
      return objectMapper.readTree(json);
    } catch (Exception e) {
      logger.error("parsing error" + e.getMessage());
    }
    return json;
  }
}
