package com.drajer.bsa.utils;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

import com.fasterxml.jackson.databind.JsonNode;
import org.hl7.fhir.r4.model.OperationOutcome;
import org.junit.Test;

public class OperationOutcomeUtilTest {

  @Test
  public void createErrorOperationOutcometest() {

    String message = "operationStringError";
    JsonNode result = (JsonNode) OperationOutcomeUtil.createErrorOperationOutcome(message);

    assertThat(result).isNotNull();
    assertThat(result.at("/issue/0/severity").asText())
        .isEqualTo(OperationOutcome.IssueSeverity.ERROR.toCode());
    assertThat(result.at("/issue/0/details/text").asText()).isEqualTo(message);
  }

  @Test
  public void createSuccessOperationOutcometest() {
    String message = "operationStringSucess";
    JsonNode result = (JsonNode) OperationOutcomeUtil.createSuccessOperationOutcome(message);

    assertThat(result).isNotNull();
    assertThat(result.at("/issue/0/severity").asText())
        .isEqualTo(OperationOutcome.IssueSeverity.INFORMATION.toCode());
    assertThat(result.at("/issue/0/code").asText())
        .isEqualTo(OperationOutcome.IssueType.INFORMATIONAL.toCode());
    assertThat(result.at("/issue/0/details/text").asText()).isEqualTo(message);
  }
}
