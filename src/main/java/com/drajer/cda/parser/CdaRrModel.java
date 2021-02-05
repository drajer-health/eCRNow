package com.drajer.cda.parser;

import java.util.ArrayList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaRrModel {

  private final Logger logger = LoggerFactory.getLogger(CdaRrModel.class);
  public static final String UNKONWN_RESPONSE_TYPE = "Unknown";

  private CdaIi eicrDocId;

  private CdaIi rrDocId;

  private CdaCode reportableStatus;

  private String reportableType;

  private String errors;

  public CdaRrModel() {}

  public CdaIi getEicrDocId() {
    return eicrDocId;
  }

  public void setEicrDocId(CdaIi eicrDocId) {
    this.eicrDocId = eicrDocId;
  }

  public String getReportableType() {
    return reportableType;
  }

  public void setReportableType(String reportableType) {
    this.reportableType = reportableType;
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

  public void setRrDocId(ArrayList<CdaIi> rrdocids) {
    if (rrdocids != null && !rrdocids.isEmpty()) {
      this.rrDocId = rrdocids.get(0);
    }
  }

  public void setEicrDocId(ArrayList<CdaIi> eicrdocids) {
    if (eicrdocids != null && !eicrdocids.isEmpty()) {
      this.eicrDocId = eicrdocids.get(0);
    }
  }

  public CdaCode getReportableStatus() {
    return reportableStatus;
  }

  public void setReportableStatus(CdaCode reportableStatus) {
    this.reportableStatus = reportableStatus;
  }
}
