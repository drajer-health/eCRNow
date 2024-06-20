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

  public static Object createErrorOperationOutcome(String message) {
    return createOperationOutcome(
        message, OperationOutcome.IssueSeverity.ERROR, OperationOutcome.IssueType.INVALID);
  }

  public static Object createSuccessOperationOutcome(String message) {
    return createOperationOutcome(
        message,
        OperationOutcome.IssueSeverity.INFORMATION,
        OperationOutcome.IssueType.INFORMATIONAL);
  }

  private static Object createOperationOutcome(
      String message, OperationOutcome.IssueSeverity severity, OperationOutcome.IssueType type) {

    IParser parser = FhirContext.forR4().newJsonParser();
    OperationOutcome operationOutcome = new OperationOutcome();
    operationOutcome.addIssue().setSeverity(severity).setCode(type).setDiagnostics(message);

    Meta meta = new Meta();
    meta.setLastUpdatedElement(new InstantType());
    meta.getLastUpdatedElement().setValue(new Date()); // Set current date/time

    operationOutcome.setMeta(meta);

    try {
      String json = parser.encodeResourceToString(operationOutcome);
      return objectMapper.readTree(json); // Convert JSON string to JsonNode
    } catch (Exception e) {
      logger.error("Parsing error: " + e.getMessage());
    }
    return null;
  }
}
