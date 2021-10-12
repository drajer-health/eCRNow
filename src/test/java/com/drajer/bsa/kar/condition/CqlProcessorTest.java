package com.drajer.bsa.kar.condition;

import com.drajer.bsa.kar.action.SubmitReport;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.ecrapp.config.SpringConfiguration;
import java.io.File;
import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Endpoint;
import org.hl7.fhir.r4.model.Expression;
import org.junit.Test;
import org.springframework.test.context.ContextConfiguration;

@ContextConfiguration(classes = SpringConfiguration.class)
public class CqlProcessorTest {

  private CqlProcessor processor = new CqlProcessor();
  private File karBundleFile =
      new File(
          "src/test/resources/Bsa/Scenarios/kars/bloodpressure/ChronicDSControllingBloodPressure-bundle.json");

  @Test
  public void testEvaluateExpression() throws Exception {
    BsaAction action = new SubmitReport();
    BsaCqlCondition bsaCondition = new BsaCqlCondition();

    CanonicalType libraryCanonical =
        new CanonicalType("http://hl7.org/fhir/us/chronic-ds/Library/ControllingBloodPressureFHIR");
    bsaCondition.setUrl(libraryCanonical.getValue());

    // Set location of eRSD bundle for loading terminology and library logic
    Endpoint libraryAndTerminologyEndpoint =
        new Endpoint()
            .setAddress(karBundleFile.getAbsolutePath())
            .setConnectionType(new Coding().setCode("hl7-fhir-files"));
    bsaCondition.setLibraryEndpoint(libraryAndTerminologyEndpoint);
    bsaCondition.setTerminologyEndpoint(libraryAndTerminologyEndpoint);
    Expression expression = new Expression();
    expression.setLanguage("text/cql");
    expression.setExpression("Numerator");
    bsaCondition.setLogicExpression(expression);
    action.addCondition(bsaCondition);
    bsaCondition.setConditionProcessor(processor);
    bsaCondition.setLogicExpression(new Expression());
    KarProcessingData kd = new KarProcessingData();
    processor.evaluateExpression(bsaCondition, action, kd);
  }
}
