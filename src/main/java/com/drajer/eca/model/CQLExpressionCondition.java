package com.drajer.eca.model;

import java.util.Set;
import org.hl7.fhir.r4.model.BooleanType;
import org.hl7.fhir.r4.model.Endpoint;
import org.hl7.fhir.r4.model.Parameters;
import org.opencds.cqf.cql.evaluator.library.LibraryProcessor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CQLExpressionCondition extends AbstractCondition {

  private String expression;

  private LibraryProcessor libraryProcessor;

  private Endpoint dataEndpoint;
  private Endpoint terminologyEndpoint;
  private Endpoint libraryEndpoint;

  private String patientId;
  private String url;

  private final Logger logger = LoggerFactory.getLogger(CQLExpressionCondition.class);

  @Override
  public Boolean evaluate(Object obj) {

    // TODO: Are we passing in data instead? That would be the "additionalData" bundle
    // TODO: The libraryProcessor is only applicable if the type of the condition is cql.identifer
    Parameters result =
        (Parameters)
            this.libraryProcessor.evaluate(
                url,
                patientId,
                null,
                libraryEndpoint,
                terminologyEndpoint,
                dataEndpoint,
                null,
                Set.of(expression));

    BooleanType value = (BooleanType) result.getParameter(expression);

    return value.getValue();
  }

  public String getExpression() {
    return expression;
  }

  public void setExpression(String expression) {
    this.expression = expression;
  }

  public Endpoint getDataEndpoint() {
    return this.dataEndpoint;
  }

  public void setDataEndpoint(Endpoint dataEndpoint) {
    this.dataEndpoint = dataEndpoint;
  }

  public Endpoint getTerminologyEndpoint() {
    return this.terminologyEndpoint;
  }

  public void setTerminologyEndpoint(Endpoint terminologyEndpoint) {
    this.terminologyEndpoint = terminologyEndpoint;
  }

  public Endpoint getLibraryEndpoint() {
    return libraryEndpoint;
  }

  public void setLibraryEndpoint(Endpoint libraryEndpoint) {
    this.libraryEndpoint = libraryEndpoint;
  }

  public String getUrl() {
    return this.url;
  }

  public void setUrl(String url) {
    this.url = url;
  }

  public String getPatientId() {
    return this.patientId;
  }

  public void setPatientId(String patientId) {
    this.patientId = patientId;
  }

  public LibraryProcessor getLibraryProcessor() {
    return this.libraryProcessor;
  }

  public void setLibraryProcessor(LibraryProcessor libraryProcessor) {
    this.libraryProcessor = libraryProcessor;
  }

  @Override
  public void print() {

    logger.info(" **** Printing CQL Condition **** ");

    printBase();

    logger.info(" Expression = {}", expression);

    logger.info(" **** End Printing CQL Condition **** ");
  }
}
