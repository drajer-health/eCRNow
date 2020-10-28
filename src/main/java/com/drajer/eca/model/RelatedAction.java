package com.drajer.eca.model;

import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RelatedAction {

  private ActionRelationshipType relationship;

  private AbstractAction abstractAction;

  private Duration duration;

  private final Logger logger = LoggerFactory.getLogger(RelatedAction.class);

  public ActionRelationshipType getRelationship() {
    return relationship;
  }

  public void setRelationship(ActionRelationshipType relationship) {
    this.relationship = relationship;
  }

  public AbstractAction getRelatedAction() {
    return abstractAction;
  }

  public void setRelatedAction(AbstractAction abstractAction) {
    this.abstractAction = abstractAction;
  }

  public Duration getDuration() {
    return duration;
  }

  public void setDuration(Duration duration) {
    this.duration = duration;
  }

  public void print() {

    if (logger.isInfoEnabled()) {
      logger.info(" *** Printing Related Actions *** ");

      if (relationship != null) logger.info(" Relationship = {}", relationship.toString());

      if (abstractAction != null) abstractAction.print();

      if (duration != null) {

        logger.info(" Duration period = {}", duration.getValue());
        logger.info(" Duration unit = {}", duration.getUnit());
      }

      logger.info("*** End Printing Related Actions *** ");
    }
  }
}
