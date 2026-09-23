package com.drajer.bsa.kar.condition;

import com.drajer.bsa.kar.model.BsaCondition;
import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.Endpoint;
// opencds CqlProcessor used via FQN to avoid collision with local CqlProcessor

/**
 * This class is used to identify Conditions with Cql Expressions.
 *
 * @author nbashyam
 */
public class BsaCqlCondition extends BsaCondition {
  private Endpoint dataEndpoint;
  private Endpoint terminologyEndpoint;
  private Endpoint libraryEndpoint;

  private String patientId;
  private String url;

  private Duration normalReportingDuration;

  public Endpoint getDataEndpoint() {
    return dataEndpoint;
  }

  public void setDataEndpoint(Endpoint dataEndpoint) {
    this.dataEndpoint = dataEndpoint;
  }

  public Endpoint getTerminologyEndpoint() {
    return terminologyEndpoint;
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

  public String getPatientId() {
    return patientId;
  }

  public void setPatientId(String patientId) {
    this.patientId = patientId;
  }

  public String getUrl() {
    return url;
  }

  public void setUrl(String url) {
    this.url = url;
  }

  public Duration getNormalReportingDuration() {
    return normalReportingDuration;
  }

  public void setNormalReportingDuration(Duration normalReportingDuration) {
    this.normalReportingDuration = normalReportingDuration;
  }

  public BsaCqlCondition() {
    setConditionProcessor(new CqlProcessor());
  }

  public org.opencds.cqf.fhir.cr.cql.CqlProcessor getLibraryEvaluationService() {
    return ((CqlProcessor) this.getConditionProcessor()).getLibraryEvaluationService();
  }

  public void setLibraryEvaluationService(org.opencds.cqf.fhir.cr.cql.CqlProcessor libraryProcessor) {
    ((CqlProcessor) this.getConditionProcessor()).setLibraryEvaluationService(libraryProcessor);
  }
}
