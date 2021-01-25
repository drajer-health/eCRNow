package com.drajer.cda.parser;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaIi extends CdaDataElement {

  private final Logger logger = LoggerFactory.getLogger(CdaDataElement.class);

  private String rootValue;
  private String extValue;

  public CdaIi() {}

  public CdaIi(String rootValue) {
    this.rootValue = rootValue;
  }

  public CdaIi(String rootValue, String extValue) {
    this(rootValue);
    this.extValue = extValue;
  }

  public void log() {

    logger.info(" *** Intance Identifier *** ");

    logger.info(" Root : {}", rootValue);
    logger.info(" Extension {}: ", extValue);
  }

  public String getRootValue() {
    return rootValue;
  }

  public void setRootValue(String rootValue) {
    this.rootValue = rootValue;
  }

  public String getExtValue() {
    return extValue;
  }

  public void setExtValue(String extValue) {
    this.extValue = extValue;
  }
}
