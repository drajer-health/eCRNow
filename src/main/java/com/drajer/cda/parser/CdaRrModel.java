package com.drajer.cda.parser;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.springframework.util.CollectionUtils;

public class CdaRrModel {

  public static final String UNKONWN_RESPONSE_TYPE = "Unknown";

  private CdaIi eicrDocId;

  private CdaIi rrDocId;

  private CdaCode reportableStatus;

  private Set<String> reportableType;

  private String errors;
  private CdaIi setId;
  private CdaIi patientId;
  private CdaIi encounterId;

  public CdaRrModel() {}

  public CdaIi getEicrDocId() {
    return eicrDocId;
  }

  public void setEicrDocId(CdaIi eicrDocId) {
    this.eicrDocId = eicrDocId;
  }

  public Set<String> getReportableType() {
    return reportableType;
  }

  public void setReportableType(Set<String> reportableType) {
    this.reportableType = reportableType;
  }

  public void addReportableType(String reportableType) {
    if (this.reportableType == null) {
      this.reportableType = new HashSet<>();
    }
    this.reportableType.add(reportableType);
  }

  public String getErrors() {
    return errors;
  }

  public void setErrors(String errors) {
    this.errors = errors;
  }

  public CdaIi getRrDocId() {
    return rrDocId;
  }

  public void setRrDocId(CdaIi rrDocId) {
    this.rrDocId = rrDocId;
  }

  public void setRrDocId(List<CdaIi> rrdocids) {
    if (!CollectionUtils.isEmpty(rrdocids)) {
      this.rrDocId = rrdocids.get(0);
    }
  }

  public void setEicrDocId(List<CdaIi> eicrdocids) {
    if (!CollectionUtils.isEmpty(eicrdocids)) {
      this.eicrDocId = eicrdocids.get(0);
    }
  }

  public CdaCode getReportableStatus() {
    return reportableStatus;
  }

  public void setReportableStatus(CdaCode reportableStatus) {
    this.reportableStatus = reportableStatus;
  }

  public CdaIi getSetId() {
    return setId;
  }

  public void setSetId(List<CdaIi> setIds) {
    if (!CollectionUtils.isEmpty(setIds)) {
      this.setId = setIds.get(0);
    }
  }

  public CdaIi getPatientId() {
    return patientId;
  }

  public void setPatientId(List<CdaIi> patientIds) {
    if (!CollectionUtils.isEmpty(patientIds)) {
      this.patientId = patientIds.get(0);
    }
  }

  public CdaIi getEncounterId() {
    return encounterId;
  }

  public void setEncounterId(List<CdaIi> encounterIds) {
    if (!CollectionUtils.isEmpty(encounterIds)) {
      this.encounterId = encounterIds.get(0);
    }
  }

  public String getPatId() {

    String patId = null;
    if (setId != null) {
      String[] ids = setId.getExtValue().split("\\|");
      patId = ids[0];
    } else if (patientId != null) {
      if (patientId.getExtValue().contains("Patient/")) {
        String[] ids = patientId.getExtValue().split("/");
        patId = ids[1];
      } else {
        patId = patientId.getExtValue();
      }
    }
    return patId;
  }

  public String getEnctId() {

    String enctId = null;
    if (setId != null) {
      String[] ids = setId.getExtValue().split("\\|");
      enctId = ids[1];
    } else if (encounterId != null) {
      if (encounterId.getExtValue().contains("Encounter/")) {
        String[] ids = encounterId.getExtValue().split("/");
        enctId = ids[1];
      } else {
        enctId = encounterId.getExtValue();
      }
    }
    return enctId;
  }
}
