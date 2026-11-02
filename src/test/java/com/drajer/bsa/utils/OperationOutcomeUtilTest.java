package com.drajer.bsa.utils;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.lang.reflect.Field;
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

  // ---------------- New tests for 100% coverage ----------------

  @Test
  public void createErrorOperationOutcome_setsCodeInvalidAndLastUpdated() {
    JsonNode result =
        (JsonNode) OperationOutcomeUtil.createErrorOperationOutcome("operationStringError");

    assertThat(result.at("/resourceType").asText()).isEqualTo("OperationOutcome");
    assertThat(result.at("/issue/0/code").asText())
        .isEqualTo(OperationOutcome.IssueType.INVALID.toCode());
    assertThat(result.at("/meta/lastUpdated").asText()).isNotEmpty();
  }

  @Test
  public void createErrorOperationOutcome_whenJsonConversionFails_returnsNull() throws Exception {
    // Covers the catch block. The private static final "objectMapper" is swapped for a mock whose
    // readTree throws, then the real one is put back so other tests are not affected.
    // (Plain Mockito here has no mockStatic, and PowerMock cannot instrument FhirContext.)
    Object realMapper = getStaticField("objectMapper");
    ObjectMapper failingMapper = mock(ObjectMapper.class);
    when(failingMapper.readTree(anyString())).thenThrow(new RuntimeException("parse failed"));

    setStaticField("objectMapper", failingMapper);
    try {
      assertThat(OperationOutcomeUtil.createErrorOperationOutcome("operationStringError")).isNull();
    } finally {
      setStaticField("objectMapper", realMapper);
    }
  }

  @Test
  public void defaultConstructor_createsInstance() {
    // OperationOutcomeUtil is a @Component with only static methods; its implicit constructor
    // is otherwise never called and counts as one uncovered line.
    assertThat(new OperationOutcomeUtil()).isNotNull();
  }

  private static Object getStaticField(String name) throws Exception {
    Field field = OperationOutcomeUtil.class.getDeclaredField(name);
    field.setAccessible(true);
    return field.get(null);
  }

  // A "static final" field cannot be changed with normal reflection on Java 17, so Unsafe is used.
  private static void setStaticField(String name, Object value) throws Exception {
    Field unsafeField = sun.misc.Unsafe.class.getDeclaredField("theUnsafe");
    unsafeField.setAccessible(true);
    sun.misc.Unsafe unsafe = (sun.misc.Unsafe) unsafeField.get(null);

    Field field = OperationOutcomeUtil.class.getDeclaredField(name);
    unsafe.putObject(unsafe.staticFieldBase(field), unsafe.staticFieldOffset(field), value);
  }
}
