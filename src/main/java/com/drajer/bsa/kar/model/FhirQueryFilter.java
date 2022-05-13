package com.drajer.bsa.kar.model;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 *
 * <h1>FhirQueryFilter</h1>
 *
 * This class stores the specific query to be executed to retrieve data based on DataRequirements
 * specified for specific PlanDefinitionActions or for Measure Evaluation purposes.
 *
 * @author nbashyam
 */
public class FhirQueryFilter {

  private final Logger logger = LoggerFactory.getLogger(FhirQueryFilter.class);

  private String queryString;

  private Boolean customized;

  public FhirQueryFilter(String st) {

    queryString = st;
    customized = false;
  }

  public FhirQueryFilter(String st, Boolean flag) {

    queryString = st;
    customized = flag;
  }

  public String getQueryString() {
    return queryString;
  }

  public void setQueryString(String queryString) {
    this.queryString = queryString;
  }

  public Boolean getCustomized() {
    return customized;
  }

  public void setCustomized(Boolean customized) {
    this.customized = customized;
  }

  public void log() {

    logger.info(" **** START Printing Fhir Query Filter ****");

    logger.info(" Query String : {}", queryString);
    logger.info(" Query Customized : {}", customized);

    logger.info(" **** FINISH Printing Fhir Query Filter ****");
  }
}
