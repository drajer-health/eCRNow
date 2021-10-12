package com.drajer.bsa.kar.condition;

import com.drajer.bsa.kar.action.SubmitReport;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.ecrapp.config.SpringConfiguration;

import org.hl7.fhir.r4.model.Expression;
import org.junit.Test;
import org.springframework.test.context.ContextConfiguration;

@ContextConfiguration(classes = SpringConfiguration.class)
public class FhirPathProcessorTest {

  private FhirPathProcessor processor = new FhirPathProcessor();

  @Test
  public void testEvaluateExpression() throws Exception {
    BsaCqlCondition bsaCondition = new BsaCqlCondition();
    bsaCondition.setConditionProcessor(processor);
    bsaCondition.setLogicExpression(new Expression());
    BsaAction action = new SubmitReport(); // TODO: mock action
    KarProcessingData kd = new KarProcessingData();
    processor.evaluateExpression(bsaCondition, action, kd);
  }
}
