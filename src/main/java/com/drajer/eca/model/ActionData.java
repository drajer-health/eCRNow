package com.drajer.eca.model;

import java.util.List;
import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.Enumerations.FHIRAllTypes;
import org.hl7.fhir.r4.model.TriggerDefinition.TriggerType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ActionData {

  private FHIRAllTypes fhirDataType;

  private TriggerType triggerType;

  private String path;

  private CanonicalType valueSet;

  private List<CanonicalType> profiles;

  private final Logger logger = LoggerFactory.getLogger(ActionData.class);

  public FHIRAllTypes getFhirDataType() {
    return fhirDataType;
  }

  public void setFhirDataType(FHIRAllTypes fhirDataType) {
    this.fhirDataType = fhirDataType;
  }

  public TriggerType getTriggerType() {
    return triggerType;
  }

  public void setTriggerType(TriggerType triggerType) {

    this.triggerType = triggerType;
  }

  public String getPath() {
    return path;
  }

  public void setPath(String path) {
    this.path = path;
  }

  public CanonicalType getValueSet() {
    return valueSet;
  }

  public void setValueSet(CanonicalType valueSet) {
    this.valueSet = valueSet;
  }

  public List<CanonicalType> getProfiles() {
    return profiles;
  }

  public void setProfiles(List<CanonicalType> profiles) {
    this.profiles = profiles;
  }

  public void print() {
    if (logger.isInfoEnabled()) {
      logger.info(" *** Printing Action Data *** ");

      if (fhirDataType != null) logger.info(" Fhir Data Type = {}", fhirDataType.toString());

      if (triggerType != null) logger.info(" Trigger Type = {}", triggerType.toString());

      logger.info(" Path  = {}", path);

      if (valueSet != null) logger.info(" ValueSet Canonical URL = {}", valueSet.getValue());

      if (profiles != null) {

        for (CanonicalType ct : profiles) {
          logger.info(" Profile Name = {}", ct.getValue());
        }
      }

      logger.info(" *** End Printing Action Data *** ");
    }
  }
}
