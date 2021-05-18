package com.drajer.bsa.model;

import com.drajer.bsa.kar.action.BsaActionStatus;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 *
 * <h1>KarProcessingData</h1>
 *
 * The KarProcessingData holds all the input and output data that is relevant during the application
 * of a KAR to a Patient.
 *
 * @author nbashyam
 */
public class KarProcessingData {

  private final Logger logger = LoggerFactory.getLogger(KarProcessingData.class);

  /** The Knowledge Artifact to be processed. */
  KnowledgeArtifact kar;

  /** The context data that was received via notification. */
  NotificationContext notificationContext;

  /**
   * The healthcare setting which will provide the necessary data to access for applying the Kar.
   */
  HealthcareSetting healthcareSetting;

  /** The raw bundle that is received as part of the notification. */
  Bundle notificationBundle;

  /**
   * The data accessed and collected from the healthcare setting for applying the KAR by Resource
   * type.
   */
  HashMap<ResourceType, Set<Resource>> fhirInputDataByType;

  /**
   * The data accessed and collected from the healthcare setting for applying the KAR by variable
   * id.
   */
  HashMap<String, Set<Resource>> fhirInputDataById;

  /**
   * The output produced by applying the KAR actions. The Key for the outer map is the Action Id
   * that produced the data. The Key for the inner map is the Resource Type of the Resource produced
   * by the Action
   */
  HashMap<String, HashMap<String, Set<Resource>>> actionOutputData;

  /** The status of each Action after its execution. */
  HashMap<String, BsaActionStatus> actionStatus;

  /** The data actually submitted to the TTP/PHA. */
  String submittedData;

  /** The response data received from the TTP/PHA. */
  String responseData;

  public Set<Resource> getResourcesByType(String type) {

    for (Map.Entry<ResourceType, Set<Resource>> entry : fhirInputDataByType.entrySet()) {

      if (entry.getKey().toString().contentEquals(type)) {
        return entry.getValue();
      }
    }

    return null;
  }

  public void addActionStatus(String id, BsaActionStatus status) {

    if (actionStatus.containsKey(id)) {

      logger.error(" Should not have the same action already ");
    } else {

      actionStatus.put(id, status);
    }
  }

  public void addResourcesByType(HashMap<ResourceType, Set<Resource>> res) {

    if (res != null && res.size() > 0) {

      logger.info(" Resource Sizes : {}", res.size());
      for (Map.Entry<ResourceType, Set<Resource>> entry : res.entrySet()) {

        if (fhirInputDataByType.containsKey(entry.getKey()))
          fhirInputDataByType.get(entry.getKey()).addAll(entry.getValue());
        else fhirInputDataByType.put(entry.getKey(), entry.getValue());
      }
    }
  }

  public void addResourcesById(HashMap<String, Set<Resource>> res) {

    if (res != null && res.size() > 0) {

      logger.info(" Resource Sizes : {}", res.size());
      for (Map.Entry<String, Set<Resource>> entry : res.entrySet()) {

        if (fhirInputDataById.containsKey(entry.getKey()))
          fhirInputDataById.get(entry.getKey()).addAll(entry.getValue());
        else fhirInputDataById.put(entry.getKey(), entry.getValue());
      }
    }
  }

  public void resetResourcesById(HashMap<String, Set<Resource>> res) {

    if (res != null && res.size() > 0) {

      logger.info(" Resource Sizes : {}", res.size());
      for (Map.Entry<String, Set<Resource>> entry : res.entrySet()) {

        if (fhirInputDataById.containsKey(entry.getKey())) {
          fhirInputDataById.get(entry.getKey()).clear();
          fhirInputDataById.put(entry.getKey(), entry.getValue());
        } else fhirInputDataById.put(entry.getKey(), entry.getValue());
      }
    }
  }

  public KarProcessingData() {

    fhirInputDataByType = new HashMap<>();
    fhirInputDataById = new HashMap<>();
    actionOutputData = new HashMap<>();
    actionStatus = new HashMap<>();
  }

  public KnowledgeArtifact getKar() {
    return kar;
  }

  public void setKar(KnowledgeArtifact kar) {
    this.kar = kar;
  }

  public HashMap<ResourceType, Set<Resource>> getFhirInputData() {
    return fhirInputDataByType;
  }

  public void setFhirInputData(HashMap<ResourceType, Set<Resource>> fhirInputData) {
    this.fhirInputDataByType = fhirInputData;
  }

  public HashMap<String, HashMap<String, Set<Resource>>> getActionOutputData() {
    return actionOutputData;
  }

  public void setActionOutputData(
      HashMap<String, HashMap<String, Set<Resource>>> actionOutputData) {
    this.actionOutputData = actionOutputData;
  }

  public HashMap<String, BsaActionStatus> getActionStatus() {
    return actionStatus;
  }

  public void setActionStatus(HashMap<String, BsaActionStatus> actionStatus) {
    this.actionStatus = actionStatus;
  }

  public NotificationContext getNotificationContext() {
    return notificationContext;
  }

  public void setNotificationContext(NotificationContext notificationContext) {
    this.notificationContext = notificationContext;
  }

  public HealthcareSetting getHealthcareSetting() {
    return healthcareSetting;
  }

  public void setHealthcareSetting(HealthcareSetting healthcareSetting) {
    this.healthcareSetting = healthcareSetting;
  }

  public Bundle getNotificationBundle() {
    return notificationBundle;
  }

  public void setNotificationBundle(Bundle notificationBundle) {
    this.notificationBundle = notificationBundle;
  }
}
