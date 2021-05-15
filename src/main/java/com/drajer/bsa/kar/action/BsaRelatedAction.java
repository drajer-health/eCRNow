package com.drajer.bsa.kar.action;

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

  private final Logger logger = LoggerFactory.getLogger(BsaRelatedAction.class);

  public BsaRelatedAction() {}

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

      logger.info(" *** Printing Related Action *** ");

      if (relationship != null) logger.info(" Relationship : {}", relationship.toString());

      if (relatedActionId != null) logger.info(" Related Action Id : {}", relatedActionId);

      if (duration != null) {

        logger.info(" Duration period : {}", duration.getValue());
        logger.info(" Duration unit : {}", duration.getUnit());
      }

      logger.info("*** End Printing Related Action *** ");
    }
  }
}
