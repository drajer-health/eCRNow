package com.drajer.cda.parser;

import com.drajer.ecrapp.service.impl.EicrServiceImpl;
import java.util.ArrayList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaRrModel {

  private final Logger logger = LoggerFactory.getLogger(EicrServiceImpl.class);

  public String eicrDocId;

  public CdaIi rrDocId;

  public String reportableType;

  public String errors;

  public CdaRrModel() {}

  public String getEicrDocId() {
    return eicrDocId;
  }

  public void setEicrDocId(String eicrDocId) {
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
    if (rrdocids != null && rrdocids.size() > 0) {
      this.rrDocId = rrdocids.get(0);
    }
  }
}
