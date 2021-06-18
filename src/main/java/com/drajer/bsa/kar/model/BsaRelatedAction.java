package com.drajer.bsa.kar.model;

import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The class captures the Relationship between two actions.
 *
 * @author nbashyam
 */
public class BsaRelatedAction {

  private ActionRelationshipType relationship;

  private String relatedActionId;

  private Duration duration;

  private BsaAction action;

  private final Logger logger = LoggerFactory.getLogger(BsaRelatedAction.class);

  public BsaRelatedAction() {
    action = null;
  }

  public BsaAction getAction() {
    return action;
  }

  public void setAction(BsaAction action) {
    this.action = action;
  }

  public ActionRelationshipType getRelationship() {
    return relationship;
  }

  public void setRelationship(ActionRelationshipType relationship) {
    this.relationship = relationship;
  }

  public Duration getDuration() {
    return duration;
  }

  public void setDuration(Duration duration) {
    this.duration = duration;
  }

  public String getRelatedActionId() {
    return relatedActionId;
  }

  public void setRelatedActionId(String relatedActionId) {
    this.relatedActionId = relatedActionId;
  }

  public void log() {

    if (logger.isInfoEnabled()) {

      logger.info(" *** Printing Related Action *** {}", relatedActionId);

      if (relationship != null) logger.info(" Relationship : {}", relationship.toString());

      if (duration != null) {

        logger.info(" Duration period : {}", duration.getValue());
        logger.info(" Duration unit : {}", duration.getCode());
      }

      logger.info("*** End Printing Related Action *** {}", relatedActionId);
    }
  }
}
