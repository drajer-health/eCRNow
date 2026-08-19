package com.drajer.bsa.utils;

import ca.uhn.fhir.context.FhirContext;
import java.util.List;
import java.util.NoSuchElementException;
import org.hl7.fhir.instance.model.api.IBase;
import org.hl7.fhir.r4.hapi.fluentpath.FhirPathR4;
import org.hl7.fhir.r4.model.Resource;

public class FhirPathEvaluator {

  private static final FhirContext fhirContext = FhirContext.forR4();
  private static FhirPathR4 fhirPath = new FhirPathR4(fhirContext);

  private FhirPathEvaluator() {
    // Utility class - private constructor to hide the implicit public one
  }

  // This method can be used to evaluate FHIRPath expressions
  public static List<IBase> evaluateFhirPath(Resource resource, String expression) {
    List<IBase> results = fhirPath.evaluate(resource, expression, IBase.class);
    if (results.isEmpty()) {
      throw new NoSuchElementException("No results found for expression: " + expression);
    }
    return results;
  }
}
