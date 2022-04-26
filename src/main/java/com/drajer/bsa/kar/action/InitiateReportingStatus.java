package com.drajer.bsa.kar.action;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class represents the status of each Initiate-Reporting-Workflow action execution .
 *
 * @author nbashyam
 */
public class InitiateReportingStatus extends BsaActionStatus {
  private final Logger logger = LoggerFactory.getLogger(InitiateReportingStatus.class);

  public InitiateReportingStatus() {
    logger.info("InitiateReportingStatus default constructor call");
  }
}
