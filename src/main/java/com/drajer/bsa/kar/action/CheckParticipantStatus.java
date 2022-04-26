package com.drajer.bsa.kar.action;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The class is used to capture the status of the CheckParticipant action.
 *
 * @author nbashyam
 */
public class CheckParticipantStatus extends BsaActionStatus {

  private final Logger logger = LoggerFactory.getLogger(CheckParticipantStatus.class);

  /** Flag to indicate the result of the participant check */
  private Boolean participantMatchStatus;

  public CheckParticipantStatus() {
    logger.info("CheckParticipantStatus default constructor call");
  }

  public Boolean getParticipantMatchStatus() {
    return participantMatchStatus;
  }

  public void setParticipantMatchStatus(Boolean participantMatchStatus) {
    this.participantMatchStatus = participantMatchStatus;
  }
}
