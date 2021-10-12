package com.drajer.bsa.kar.condition;

import com.drajer.bsa.kar.model.BsaCondition;
import org.hl7.fhir.r4.model.Parameters;

/**
 * This class is used to identify Conditions of type FHIRPath.
 *
 * @author nbashyam
 */
public class BsaFhirPathCondition extends BsaCondition {

  Parameters inputParameters = new Parameters();

  public BsaFhirPathCondition() {

    setConditionProcessor(new FhirPathProcessor());
  }

  public Parameters getInputParameters() {
    return inputParameters;
  }

  public void setInputParameters(Parameters inputParameters) {
    this.inputParameters = inputParameters;
  }
}
