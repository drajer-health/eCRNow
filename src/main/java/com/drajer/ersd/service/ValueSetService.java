package com.drajer.ersd.service;

import org.hl7.fhir.r4.model.TriggerDefinition;
import org.hl7.fhir.r4.model.ValueSet;

public interface ValueSetService {

  public void createValueSet(ValueSet valueSet);

  public void createValueSetGrouper(ValueSet valueSet);

  public void createPlanDefinitionAction(TriggerDefinition triggerDefinition);
}
