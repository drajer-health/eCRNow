package com.drajer.bsa.kar.condition;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.BsaCondition;
import com.drajer.bsa.model.KarProcessingData;
import java.util.HashSet;
import java.util.Set;

import org.hl7.fhir.r4.model.BooleanType;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Parameters;
import org.hl7.fhir.r4.model.Parameters.ParametersParameterComponent;
import org.hl7.fhir.r4.model.ResourceType;
import org.opencds.cqf.cql.evaluator.library.LibraryProcessor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CqlProcessor implements BsaConditionProcessor {
  private static final Logger logger = LoggerFactory.getLogger(CqlProcessor.class);

  private LibraryProcessor libraryProcessor;

  @Override
  public Boolean evaluateExpression(
      BsaCondition cond, BsaAction act, KarProcessingData kd, EhrQueryService ehrService) {
    Set<String> expressions = new HashSet<>();
    expressions.add(cond.getLogicExpression().getExpression());
    if (!(cond instanceof BsaCqlCondition)) {
      logger.error("Expected a BsaCqlCondition, but found:{} ", cond);
    }
    BsaCqlCondition cqlCondition = (BsaCqlCondition) cond;
    Parameters parameters = null;
    for (BundleEntryComponent entry : kd.getNotificationBundle().getEntry()) {
      if (entry.hasResource()
          && entry.getResource().getResourceType().equals(ResourceType.Encounter)) {
        parameters = new Parameters();
        ParametersParameterComponent paramComponent = new ParametersParameterComponent();
        paramComponent.setResource(entry.getResource());
        paramComponent.setName("Triggering Encounter");
        parameters.addParameter(paramComponent);
      }
    }
    Parameters result =
        (Parameters)
            this.libraryProcessor.evaluate(
                cqlCondition.getUrl(),
                cqlCondition.getPatientId(),
                parameters,
                cqlCondition.getLibraryEndpoint(),
                cqlCondition.getTerminologyEndpoint(),
                cqlCondition.getDataEndpoint(),
                kd.getInputResourcesAsBundle(),
                expressions);

    BooleanType value =
        (BooleanType) result.getParameter(cond.getLogicExpression().getExpression());

    return value.getValue();
  }

  public LibraryProcessor getLibraryProcessor() {
    return libraryProcessor;
  }

  public void setLibraryProcessor(LibraryProcessor libraryProcessor) {
    this.libraryProcessor = libraryProcessor;
  }

  @Override
  public Boolean evaluateExpression(
      BsaCondition cond, Parameters params, EhrQueryService ehrService) {

    logger.error(" Unable to evalue expression as this is not implementd.");

    return false;
  }
}
