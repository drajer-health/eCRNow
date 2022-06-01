package com.drajer.bsa.kar.model;

import org.hl7.fhir.r4.model.ResourceType;
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

  private String dataReqId;

  private String queryString;

  private Boolean customized;

  private ResourceType resourceType;

  private String relatedDataId;

  public FhirQueryFilter(String st) {

    queryString = st;
    customized = false;
  }

  public FhirQueryFilter(String st, Boolean flag, ResourceType rtype) {

    queryString = st;
    customized = flag;
    resourceType = rtype;
  }

  public FhirQueryFilter() {
    customized = false;
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

  public ResourceType getResourceType() {
    return resourceType;
  }

  public void setResourceType(ResourceType resourceType) {
    this.resourceType = resourceType;
  }

  public String getDataReqId() {
    return dataReqId;
  }

  public void setDataReqId(String dataReqId) {
    this.dataReqId = dataReqId;
  }

  public String getRelatedDataId() {
    return relatedDataId;
  }

  public void setRelatedDataId(String relatedDataId) {
    this.relatedDataId = relatedDataId;
  }

  public void log() {

    logger.info(" **** START Printing Fhir Query Filter ****");

    logger.info(" Data Req Id : {}", dataReqId);
    logger.info(" Query String : {}", queryString);
    logger.info(" Query Customized : {}", customized);
    logger.info(" Resource Type : {}", resourceType);
    logger.info(" Related Data Id : {}", relatedDataId);

    logger.info(" **** FINISH Printing Fhir Query Filter ****");
  }
}
