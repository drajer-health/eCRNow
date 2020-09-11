package com.drajer.eca.model;

import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RelatedAction {

	private ActionRelationshipType relationship;

	private AbstractAction relatedAction;

	private Duration duration;

	private final Logger logger = LoggerFactory.getLogger(RelatedAction.class);

	public ActionRelationshipType getRelationship() {
		return relationship;
	}

	public void setRelationship(ActionRelationshipType relationship) {
		this.relationship = relationship;
	}

	public AbstractAction getRelatedAction() {
		return relatedAction;
	}

	public void setRelatedAction(AbstractAction relatedAction) {
		this.relatedAction = relatedAction;
	}

	public Duration getDuration() {
		return duration;
	}

	public void setDuration(Duration duration) {
		this.duration = duration;
	}

	public void print() {

		logger.info(" *** Printing Related Actions *** ");

		if (relationship != null)
			logger.info(" Relationship = {}", relationship.toString());

		if (relatedAction != null)
			relatedAction.print();

		if (duration != null) {

			logger.info(" Duration period = {}", duration.getValue());
			logger.info(" Duration unit = {}", duration.getUnit());
		}

		logger.info("*** End Printing Related Actions *** ");
	}

}
